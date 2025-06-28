//===- VulkanEarly.cpp - Vulkan early pass --------------------------------===//
//
//  Flo's Open libRary (floor)
//  Copyright (C) 2004 - 2025 Florian Ziesche
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; version 2 of the License only.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
//===----------------------------------------------------------------------===//
//
// This file fixes certain post-codegen issues.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/GlobalsModRef.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/InitializePasses.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/ConstantRange.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"
#include "llvm/PassRegistry.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/LibFloor.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/LoopUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/LowerMemIntrinsics.h"
#include "llvm/Transforms/LibFloor/AddressSpaceFix.h"
#include "llvm/Transforms/LibFloor/FloorUtils.h"
#include <algorithm>
#include <cstdarg>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <deque>
#include <array>
using namespace llvm;

#define DEBUG_TYPE "VulkanEarly"

#if 1
#define DBG(x)
#else
#define DBG(x) x
#endif

namespace {
	struct VulkanEarlyArgBufferFunctionClone : public ModulePass {
		static char ID; // Pass identification, replacement for typeid
		
		Module* M { nullptr };
		LLVMContext* ctx { nullptr };
		
		VulkanEarlyArgBufferFunctionClone() : ModulePass(ID) {
			initializeVulkanFinalModuleCleanupPass(*PassRegistry::getPassRegistry());
		}
		
		void getAnalysisUsage(AnalysisUsage &AU) const override {
			AU.addRequired<AAResultsWrapperPass>();
			AU.addRequired<GlobalsAAWrapperPass>();
			AU.addRequired<AssumptionCacheTracker>();
			AU.addRequired<TargetLibraryInfoWrapperPass>();
		}
		
		// this clones the specified function and adjusts it for Vulkan argument buffer use, assuming the first argument is the argument buffer
		// NOTE: ideally, we would already do this during code gen, but at that point we don't actually know what the function is used for,
		//       i.e. it might be used for both argument buffers and normal structs -> would need to instantiate it twice, but this is non-trivial
		// NOTE: this may happen for member functions of a struct/class (i.e. the "this" pointer), which is later used as an argument buffer element type
		static Function* clone_func_for_arg_buffer(Pass& pass, Function* F, BitCastInst* arg_buffer_arg) {
			std::vector<Type *> ArgTypes;
			ValueToValueMapTy VMap;
			ClonedCodeInfo *CodeInfo = nullptr;
			
			// replace first arg type + copy all others
			auto orig_arg_buffer_arg = arg_buffer_arg->getOperand(0);
			ArgTypes.push_back(orig_arg_buffer_arg->getType());
			for (uint32_t i = 1, count = F->arg_size(); i < count; ++i) {
				ArgTypes.push_back(F->getArg(i)->getType());
			}
			
			// create new function
			auto FTy = FunctionType::get(F->getFunctionType()->getReturnType(), ArgTypes,
										 F->getFunctionType()->isVarArg());
			auto cloned_func = Function::Create(FTy, F->getLinkage(), F->getAddressSpace(),
												F->getName() + ".argbuf_clone", F->getParent());
			
			// loop over the arguments, copying the names of the mapped arguments over
			Function::arg_iterator DestI = cloned_func->arg_begin();
			for (const Argument &I : F->args()) {
				if (VMap.count(&I) == 0) {
					DestI->setName(I.getName());
					VMap[&I] = &*DestI++;
				}
			}
			
			SmallVector<ReturnInst *, 8> Returns; // Ignore returns cloned.
			CloneFunctionInto(cloned_func, F, VMap, CloneFunctionChangeType::LocalChangesOnly,
							  Returns, "", CodeInfo);
			
			// fix up GEPs
			auto first_arg = cloned_func->getArg(0);
			libfloor_utils::for_all_users(*first_arg, [&pass, &first_arg](User& user) {
				if (auto instr = dyn_cast<Instruction>(&user)) {
					// TODO: do we need to handle load/store/PHI/select/BitCast/call or are these always GEPs? -> should always be GEP when using a struct?
					if (auto GEP = dyn_cast_or_null<GetElementPtrInst>(instr); GEP) {
						// create new GEP with updated type
						
						// we should always have a pointer to a struct type here, dereferencing a struct element
						// -> must have at least 2 indices
						assert(GEP->getNumIndices() >= 2);
						assert(GEP->getSourceElementType()->isStructTy());
						assert(first_arg->getType()->getPointerElementType()->isStructTy());
						
						// load the initial argument buffer pointer
						SmallVector<Value*, 2> init_ld_indices {{
							GEP->getOperand(1),
							GEP->getOperand(2)
						}};
						auto init_gep = llvm::GetElementPtrInst::Create(first_arg->getType()->getPointerElementType(), first_arg, init_ld_indices,
																		first_arg->getName() + ".argbuf_init_gep", GEP);
						init_gep->setDebugLoc(GEP->getDebugLoc());
						if (GEP->isInBounds()) {
							init_gep->setIsInBounds();
						}
						
						auto init_ld = new LoadInst(init_gep->getResultElementType(), init_gep, first_arg->getName() + ".argbuf_init_ld", false, GEP);
						init_ld->setDebugLoc(GEP->getDebugLoc());
						assert(init_ld->getType()->isPointerTy());
						
						llvm::Instruction* repl_instr = init_ld;
						if (GEP->getNumIndices() >= 3) {
							// if this is further looking up *something*, we need to add another GEP
							
							// replacement GEP now consists of the remaining indices into this pointer
							// NOTE: we may run into incorrect address spaces here, but this will later be fixed up by AddressSpaceFix
							static constexpr const uint32_t SPIRAS_StorageBuffer = 12;
							SmallVector<Value*, 8> indices;
							if (init_ld->getType()->getPointerAddressSpace() != SPIRAS_StorageBuffer) {
								// for non-global/StorageBuffer data, we need to add an additional 0 prefix index
								indices.emplace_back(ConstantInt::get(Type::getInt32Ty(GEP->getContext()), 0));
							}
							for (uint32_t i = 2, count = GEP->getNumIndices(); i < count; ++i) {
								indices.emplace_back(GEP->getOperand(1 + i));
							}
							auto repl_gep = llvm::GetElementPtrInst::Create(init_ld->getType()->getPointerElementType(), init_ld, indices,
																			GEP->getName() + ".argbuf_gep", GEP);
							if (GEP->isInBounds()) {
								repl_gep->setIsInBounds();
							}
							repl_gep->setDebugLoc(GEP->getDebugLoc());
							repl_instr = repl_gep;
						}
						
						// bitcast if the (final) type doesn't match (yet)
						const auto address_space = repl_instr->getType()->getPointerAddressSpace();
						BitCastInst* bc = nullptr;
						if (repl_instr->getType() != GEP->getType()) {
							auto GEP_ptr_type = dyn_cast_or_null<PointerType>(GEP->getType());
							auto repl_ptr_type = dyn_cast_or_null<PointerType>(repl_instr->getType());
							assert(GEP_ptr_type && repl_ptr_type);
							if (GEP_ptr_type->getPointerElementType() != repl_ptr_type->getPointerElementType()) {
								// we can't perform an address space cast here and bitcasts don't allow address space changes
								// -> create a bitcast using the new address space, but immediately mutate it back to the old address space
								auto old_gep_type_new_as = PointerType::get(GEP_ptr_type->getPointerElementType(), repl_ptr_type->getPointerAddressSpace());
								bc = new BitCastInst(repl_instr, old_gep_type_new_as, ".argbuf_gep_bc", GEP);
								bc->mutateType(GEP_ptr_type);
								bc->setDebugLoc(GEP->getDebugLoc());
								repl_instr = bc;
							}
						}
						
						GEP->replaceAllUsesWith(repl_instr, true /* allow address space change */);
						GEP->eraseFromParent();
						
						// always fix up the address spaces of all instruction users, including calls (we can do this here, since this is a module pass that has the proper analysis)
						std::vector<ReturnInst*> returns;
						libfloor_utils::for_all_instruction_users(*repl_instr, [&pass, &repl_instr, &address_space, &returns, ctx = &repl_instr->getContext()](Instruction& I) {
							fix_instruction_users_with_calls(pass, *ctx, I, *repl_instr, address_space, false, returns);
						});
						
						// if we needed to create a bitcast, we can/must now change the address space back to the new correct one
						if (bc) {
							bc->mutateType(PointerType::get(bc->getDestTy()->getPointerElementType(), address_space));
						}
					} else {
						assert(false && "unhandled arg user");
					}
				} else {
					assert(false && "arg user is not an instruction");
				}
			});
			
			// the above instruction replacements may have changed the function return type -> update the function
			llvm::Type* updated_ret_type = nullptr;
			for (auto&& BB : *cloned_func) {
				if (auto ret = dyn_cast_or_null<ReturnInst>(BB.getTerminator()); ret && ret->getNumOperands() > 0) {
					if (!updated_ret_type) {
						updated_ret_type = ret->getOperand(0)->getType();
					}
					assert(updated_ret_type == ret->getOperand(0)->getType());
				}
			}
			if (updated_ret_type && cloned_func->getReturnType() != updated_ret_type) {
				cloned_func->mutateFunctionType(FunctionType::get(updated_ret_type, ArgTypes, F->getFunctionType()->isVarArg()));
			}
			
			return cloned_func;
		}
		
		bool runOnModule(Module& Mod) override {
			M = &Mod;
			ctx = &M->getContext();
			
			// iterate over *all* instructions (need to handle both entry-point and non-entry-point functions here)
			bool module_modified = false;
			for (auto&& func : Mod) {
				for (auto&& BB : func) {
					for (auto&& I : BB) {
						if (auto CI = dyn_cast_or_null<CallInst>(&I); CI && CI->getNumOperands() > 0) {
							// when encountering a function where the first argument is flagged as a Vulkan argument buffer,
							// but the function isn't actually adjusted for argument buffer use,
							// we need to do this now by cloning and adjusting the function
							if (auto bc = dyn_cast_or_null<BitCastInst>(CI->getOperand(0)); bc) {
								auto annotation_md = bc->getMetadata(llvm::LLVMContext::MD_annotation);
								if (annotation_md && annotation_md->getNumOperands() > 0) {
									auto annotation_str = dyn_cast_or_null<llvm::MDString>(annotation_md->getOperand(0));
									if (annotation_str && annotation_str->getString().equals("vulkan_arg_buffer")) {
										auto new_called_func = clone_func_for_arg_buffer(*this, CI->getCalledFunction(), bc);
										CI->setCalledFunction(new_called_func);
										CI->setArgOperand(0, bc->getOperand(0));
										module_modified = true;
									}
								}
							}
						}
					}
				}
			}
			return module_modified;
		}
		
	};
	
}

char VulkanEarlyArgBufferFunctionClone::ID = 0;
ModulePass *llvm::createVulkanEarlyArgBufferFunctionClonePass() {
	return new VulkanEarlyArgBufferFunctionClone();
}
INITIALIZE_PASS_BEGIN(VulkanEarlyArgBufferFunctionClone, "VulkanEarly arg buffer function clone", "VulkanEarly arg buffer function clone Pass", false, false)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_DEPENDENCY(GlobalsAAWrapperPass)
INITIALIZE_PASS_DEPENDENCY(AssumptionCacheTracker)
INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
INITIALIZE_PASS_END(VulkanEarlyArgBufferFunctionClone, "VulkanFinal arg buffer function clone", "VulkanEarly arg buffer function clone Pass", false, false)
