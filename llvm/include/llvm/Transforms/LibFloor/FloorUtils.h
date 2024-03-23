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

#include "llvm/IR/Value.h"

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

} // namespace libfloor_utils

#endif
