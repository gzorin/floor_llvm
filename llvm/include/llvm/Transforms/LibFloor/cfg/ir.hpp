/* Copyright (c) 2019-2022 Hans-Kristian Arntzen for Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
//==-----------------------------------------------------------------------===//
//
// dxil-spirv CFG structurizer adopted for LLVM use
// ref: https://github.com/HansKristian-Work/dxil-spirv
// @ 51f9c11f6a3ce01ef51c859f40d663eb3bb5883b
//
//===----------------------------------------------------------------------===//

#pragma once

#include <assert.h>
#include <initializer_list>
#include <stdint.h>
#include <vector>
#include <optional>
#include "llvm/IR/CFG.h"

// A simple IR representation which allows the CFGStructurizer to do some simple
// rewrites of blocks, PHI nodes in particular.

namespace llvm {

enum class MergeType { None, Loop, Selection };

class ConstantInt;
struct CFGNode;

struct MergeInfo {
  MergeType merge_type = MergeType::None;
  CFGNode *merge_block = nullptr;
  CFGNode *continue_block = nullptr;
};

struct IncomingValue {
  CFGNode *block = nullptr;
  Value *value = nullptr;
};

struct PHI {
  PHINode *phi = nullptr;
  bool relaxed = false;
  std::vector<IncomingValue> incoming;
};

struct Terminator {
  enum class Type { Unreachable, Branch, Condition, Switch, Return, Kill };

  // NOTE: this may be nullptr for newly created conditions
  Instruction *terminator = nullptr;

  Type type = Type::Unreachable;

  // Branch
  CFGNode *direct_block = nullptr;

  // Conditional Branch and Switch
  Value *condition = nullptr;

  // Condition
  CFGNode *true_block = nullptr;
  CFGNode *false_block = nullptr;

  // Switch
  struct Case {
    CFGNode *node = nullptr;
    uint64_t global_order = 0;
    ConstantInt *value = nullptr;
    bool is_default = false;
  };
  std::vector<Case> cases;

  // Return
  Value *return_value = nullptr;
};

struct IRBlock {
  std::vector<PHI> phi;
  std::vector<Instruction *> operations;
  MergeInfo merge_info;
  Terminator terminator;
};

} // namespace llvm
