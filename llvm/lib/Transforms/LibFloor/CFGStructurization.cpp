//===-- CFGStructurization.cpp - CFG Structurizer -------------------------===//
//
//  Flo's Open libRary (floor)
//  Copyright (C) 2004 - 2022 Florian Ziesche
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
//==-----------------------------------------------------------------------===//
//
// Transforms the CFG into a structurized CFG.
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
#include "llvm/Analysis/LoopInfo.h"
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
#include "llvm/IR/Verifier.h"
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
#include <algorithm>
#include <cstdarg>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <deque>
#include <array>

#include "llvm/Transforms/LibFloor/cfg/cfg_structurizer.hpp"
#include "llvm/Transforms/LibFloor/cfg/cfg_translator.hpp"

using namespace llvm;

#define DEBUG_TYPE "cfg-structurization"

#if 1
#define DBG(x)
#else
#define DBG(x) x
#endif

namespace {
	class CFGStructurization : public FunctionPass {
	public:
		static char ID; // Pass identification, replacement for typeid
		
		Module* M { nullptr };
		LLVMContext* ctx { nullptr };
		Function* func { nullptr };
		
		bool was_modified { false };

		CFGStructurization() : FunctionPass(ID) {
			initializeCFGStructurizationPass(*PassRegistry::getPassRegistry());
		}
		
		StringRef getPassName() const override {
			return "CFG structurization";
		}
		
		bool runOnFunction(Function &F) override {
			M = F.getParent();
			ctx = &M->getContext();
			func = &F;
			was_modified = run_structurization(F);
			return was_modified;
		}
		
		bool run_structurization(Function &F) {
			// skip if we only have a single block that returns
			if (auto ret_instr = dyn_cast_or_null<ReturnInst>(F.getEntryBlock().getTerminator())) {
				return false;
			}
			was_modified = true;
			
			CFGNodePool node_pool(*ctx, F);
			cfg_translator translator(F, *ctx, node_pool);
			
			CFGStructurizer structurizer(translator.get_entry_block(), node_pool, F, *ctx);
			structurizer.run();
			translator.cfg_to_llvm_ir(structurizer.get_entry_block(), true);
			
#if !defined(NDEBUG) || 1 // TODO: disable in release mode later on - for now, always keep it on
			const auto verify_failed = verifyFunction(F, &errs());
			if (verify_failed) {
				errs().flush();
				assert(!verify_failed && "verification failed");
			}
#endif
			
			return was_modified;
		}
	};

}

char CFGStructurization::ID = 0;
FunctionPass *llvm::createCFGStructurizationPass() {
	return new CFGStructurization();
}
INITIALIZE_PASS_BEGIN(CFGStructurization, "CFGStructurization", "CFGStructurization Pass", false, false)
INITIALIZE_PASS_END(CFGStructurization, "CFGStructurization", "CFGStructurization Pass", false, false)
