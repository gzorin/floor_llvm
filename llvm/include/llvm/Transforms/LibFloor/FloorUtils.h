//===- FloorUtils.h - libfloor utility functions --------------------------===//
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
// libfloor utility functions
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_LIBFLOOR_FLOORUTILS_H
#define LLVM_TRANSFORMS_LIBFLOOR_FLOORUTILS_H

#include <functional>
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"

namespace libfloor_utils {

//! execute specified "func_cb(User&)" on all users of "val"
template <typename F>
static inline void for_all_users(llvm::Value& val, F&& func_cb /* void(User&) */) {
	// gather direct and single-indirect users
	std::vector<llvm::User*> users;
	for (auto user : val.users()) {
		users.emplace_back(user);
		if (auto const_expr = dyn_cast<llvm::ConstantExpr>(user)) {
			// allow single recursion into constant expression
			for (auto ce_user : const_expr->users()) {
				users.emplace_back(ce_user);
			}
		}
	}
	// call user callback for all users
	for (auto& user : users) {
		func_cb(*user);
	}
}

//! execute specified "func_cb(const User&)" on all users of "val" (const variant)
template <typename F>
static inline void for_all_users(const llvm::Value& val, F&& func_cb /* void(const User&) */) {
	// gather direct and single-indirect users
	std::vector<const llvm::User*> users;
	for (const auto user : val.users()) {
		users.emplace_back(user);
		if (const auto const_expr = dyn_cast<const llvm::ConstantExpr>(user)) {
			// allow single recursion into constant expression
			for (const auto ce_user : const_expr->users()) {
				users.emplace_back(ce_user);
			}
		}
	}
	// call user callback for all users
	for (const auto& user : users) {
		func_cb(*user);
	}
}

//! execute specified "func_cb(Instruction&)" on all instruction users of "val"
template <typename F>
static inline void for_all_instruction_users(llvm::Value& val, F&& func_cb /* void(Instruction&) */) {
	// gather direct and single-indirect instruction users
	std::vector<llvm::Instruction*> instr_users;
	for (auto user : val.users()) {
		if (auto instr = dyn_cast<llvm::Instruction>(user)) {
			instr_users.emplace_back(instr);
		} else if (auto const_expr = dyn_cast<llvm::ConstantExpr>(user)) {
			// allow single recursion into constant expression
			for (auto ce_user : const_expr->users()) {
				if (auto ce_instr = dyn_cast<llvm::Instruction>(ce_user)) {
					instr_users.emplace_back(ce_instr);
				}
			}
		}
	}
	// call user callback for all instructions
	for (auto& instr : instr_users) {
		func_cb(*instr);
	}
}

//! execute specified "func_cb(const Instruction&)" on all instruction users of "val" (const variant)
template <typename F>
static inline void for_all_instruction_users(const llvm::Value& val, F&& func_cb /* void(const Instruction&) */) {
	// gather direct and single-indirect instruction users
	std::vector<const llvm::Instruction*> instr_users;
	for (const auto user : val.users()) {
		if (const auto instr = dyn_cast<const llvm::Instruction>(user)) {
			instr_users.emplace_back(instr);
		} else if (const auto const_expr = dyn_cast<const llvm::ConstantExpr>(user)) {
			// allow single recursion into constant expression
			for (const auto ce_user : const_expr->users()) {
				if (const auto ce_instr = dyn_cast<const llvm::Instruction>(ce_user)) {
					instr_users.emplace_back(ce_instr);
				}
			}
		}
	}
	// call user callback for all instructions
	for (const auto& instr : instr_users) {
		func_cb(*instr);
	}
}

//! tries to simplify the specified constant integer value to 32-bit,
//! returns the simplified value if one could be created, nullptr otherwise
static inline llvm::ConstantInt* simplify_const_integer_to_32bit(llvm::ConstantInt& const_val) {
	// integer constant -> use signed 32-bit instead if constant is small enough
	auto const_value = const_val.getZExtValue();
	if (const_val.getBitWidth() > 32 && const_value <= 0x7FFF'FFFFull) {
		return llvm::ConstantInt::get(llvm::Type::getInt32Ty(const_val.getContext()), int32_t(const_value));
	}
	return nullptr;
}

//! tries to simplify the specified integer value to 32-bit,
//! returns the simplified value if one could be created, nullptr otherwise
//! if "erase_unused_origin" is true, any originating values that are no longer used will be erased
//! if "func_cb" is specified, it will be called with the new simplified value prior to the "erase unused" check
static inline llvm::Value* simplify_integer_to_32bit(llvm::Value& val, const bool erase_unused_origin = false,
													 std::function<void(llvm::Value*)> func_cb = {}) {
	if (!val.getType()->isIntegerTy()) {
		return nullptr;
	}
	
	if (auto const_val = dyn_cast_or_null<llvm::ConstantInt>(&val); const_val) {
		auto new_const_val = simplify_const_integer_to_32bit(*const_val);
		if (new_const_val && func_cb) {
			func_cb(new_const_val);
		}
		return new_const_val;
	}
	
	// dynamic integer value
	if (auto cast_instr = dyn_cast_or_null<llvm::CastInst>(&val); cast_instr) {
		switch (cast_instr->getOpcode()) {
			case llvm::Instruction::BitCast: {
				break;
			}
			case llvm::Instruction::SExt:
			case llvm::Instruction::ZExt: {
				// if the original is a 32-bit integer, use that instead
				auto orig_int = cast_instr->getOperand(0);
				if (orig_int->getType()->isIntegerTy() &&
					((const llvm::IntegerType*)orig_int->getType())->getBitWidth() <= 32) {
					if (func_cb) {
						func_cb(orig_int);
					}
					
					// kill cast if we are the only user (left)
					if (erase_unused_origin && cast_instr->getNumUses() == 0) {
						cast_instr->eraseFromParent();
					}
					
					return orig_int;
				}
				break;
			}
			default:
				// -> keep as-is
				break;
		}
	}
	return nullptr;
}

//! simplifies GEP indices:
//!  * convert constant integers into i32-typed constants if possible
//!  * remove i32 -> i64 casts of indices (use original i32 index directly)
//! returns true if GEP was modified
static inline bool simplify_gep_indices(llvm::LLVMContext& ctx, llvm::GetElementPtrInst &I) {
	using namespace llvm;
	
	bool did_modify = false;
	for (auto& op : I.operands()) {
		if (op->getType()->isIntegerTy() && !op->getType()->isIntegerTy(32)) {
			// using the callback rather than the return value to allow for proper unused removal
			simplify_integer_to_32bit(*op, true /* kill unused */,
									  [&op, &did_modify](llvm::Value* new_op) {
				op.set(new_op);
				did_modify = true;
			});
		}
	}
	return did_modify;
}
// TODO: should do the same for extractelement/insertelement/extractvalue/insertvalue

} // namespace libfloor_utils

#endif
