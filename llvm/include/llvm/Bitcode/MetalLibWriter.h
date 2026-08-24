//===- llvm/Bitcode/MetalLibWriter.h - Metal library entry building ------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_BITCODE_METALLIBWRITER_H
#define LLVM_BITCODE_METALLIBWRITER_H

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/STLFunctionalExtras.h"
#include <cstdint>
#include <memory>

namespace llvm {

class Function;
class GlobalValue;
class Module;

/// Build the single-entry module WriteMetalLibToFile packages for one .metallib
/// container entry: a clone of M restricted by ShouldCloneDefinition (see
/// CloneModule), pruned to `entry` via internalize + GlobalOpt + GlobalDCE, with
/// the writer's per-entry ABI/metadata fixups applied (calling convention,
/// entry-point/sampler-state metadata lists, llvm.ident, floor.* metadata,
/// local/constant address space GV alignment and initializers).
std::unique_ptr<Module>
buildEntryModule(const Module &M, const Function *entry,
                  function_ref<bool(const GlobalValue *)> ShouldCloneDefinition,
                  uint32_t target_air_version);

/// GlobalValues that must survive every per-entry buildEntryModule() clone of
/// M regardless of a specific entry's own reachability: functions in the
/// "air.static_init" section run automatically before any entry point in the
/// program executes, so they have no call edge from any entry to discover them
/// by. Rooted at those functions directly and, since a static-init function's
/// only real (non-metadata) use is usually the pointer to it stored in
/// @llvm.global_ctors's initializer, at @llvm.global_ctors itself -- without
/// that array surviving too, the function looks unused after internalize and
/// GlobalOpt deletes it outright.
DenseSet<const GlobalValue *> computeStaticInitRoots(const Module &M);

} // end namespace llvm

#endif
