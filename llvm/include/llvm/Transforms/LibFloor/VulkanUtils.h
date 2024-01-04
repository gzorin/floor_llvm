//===- VulkanSampling.h - Vulkan-specific sampler/image info --------------===//
//
//  Flo's Open libRary (floor)
//  Copyright (C) 2004 - 2024 Florian Ziesche
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
// Vulkan transform utilities
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_LIBFLOOR_VULKANUTILS_H
#define LLVM_TRANSFORMS_LIBFLOOR_VULKANUTILS_H

namespace vulkan_utils {

//! simplifies GEP indices:
//!  * convert constant integers into i32-typed constants if possible
//!  * remove i32 -> i64 casts of indices (use original i32 index directly)
//! returns true if GEP was modified
static inline bool simplify_gep_indices(llvm::LLVMContext& ctx, llvm::GetElementPtrInst &I) {
	using namespace llvm;
	
	bool did_modify = false;
	for (auto& op : I.operands()) {
		if (op->getType()->isIntegerTy() && !op->getType()->isIntegerTy(32)) {
			if (const auto const_idx_op = dyn_cast_or_null<ConstantInt>(op)) {
				// integer constant -> use signed 32-bit instead if constant is small enough
				const auto const_val = const_idx_op->getValue().getZExtValue();
				if (const_val <= 0x7FFF'FFFFull) {
					op.set(ConstantInt::get(Type::getInt32Ty(ctx),
											(int32_t)const_idx_op->getValue().getZExtValue()));
					did_modify = true;
				}
			} else {
				// dynamic integer value
				if (auto cast_instr = dyn_cast_or_null<CastInst>(op); cast_instr) {
					switch (cast_instr->getOpcode()) {
						case llvm::Instruction::BitCast: {
							break;
						}
						case llvm::Instruction::SExt:
						case llvm::Instruction::ZExt: {
							// if the original is a 32-bit integer, use that instead
							auto orig_int = cast_instr->getOperand(0);
							if (orig_int->getType()->isIntegerTy()) {
								const auto orig_int_type = (const llvm::IntegerType*)orig_int->getType();
								if (orig_int_type->getBitWidth() <= 32) {
									op.set(orig_int);
									did_modify = true;
									
									// kill cast if we are the only user (left)
									if (cast_instr->getNumUses() == 0) {
										cast_instr->eraseFromParent();
									}
								}
							}
							break;
						}
						default:
							// -> keep as-is
							break;
					}
				}
			}
		}
	}
	return did_modify;
}

// TODO: should do the same for extractelement/insertelement/extractvalue/insertvalue

} // namespace vulkan_utils

#endif
