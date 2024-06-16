//===- BitcodeWriterPass140.cpp - Bitcode writing pass --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// BitcodeWriterPass 14.0 implementation.
//
//===----------------------------------------------------------------------===//

#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/Analysis/ModuleSummaryAnalysis.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
using namespace llvm;

PreservedAnalyses BitcodeWriterPass140::run(Module &M, ModuleAnalysisManager &AM) {
  const ModuleSummaryIndex *Index =
      EmitSummaryIndex ? &(AM.getResult<ModuleSummaryIndexAnalysis>(M))
                       : nullptr;
  WriteBitcodeToFile140(M, OS, ShouldPreserveUseListOrder, Index, EmitModuleHash);
  return PreservedAnalyses::all();
}

namespace {
  class WriteBitcodePass140 : public ModulePass {
    raw_ostream &OS; // raw_ostream to print on
    bool ShouldPreserveUseListOrder;
    bool EmitSummaryIndex;
    bool EmitModuleHash;

  public:
    static char ID; // Pass identification, replacement for typeid
    WriteBitcodePass140() : ModulePass(ID), OS(dbgs()) {
      initializeWriteBitcodePass140Pass(*PassRegistry::getPassRegistry());
    }

    explicit WriteBitcodePass140(raw_ostream &o, bool ShouldPreserveUseListOrder,
                                 bool EmitSummaryIndex, bool EmitModuleHash)
        : ModulePass(ID), OS(o),
          ShouldPreserveUseListOrder(ShouldPreserveUseListOrder),
          EmitSummaryIndex(EmitSummaryIndex), EmitModuleHash(EmitModuleHash) {
      initializeWriteBitcodePass140Pass(*PassRegistry::getPassRegistry());
    }

    StringRef getPassName() const override { return "Bitcode 14.0 Writer"; }

    bool runOnModule(Module &M) override {
      const ModuleSummaryIndex *Index =
          EmitSummaryIndex
              ? &(getAnalysis<ModuleSummaryIndexWrapperPass>().getIndex())
              : nullptr;
      WriteBitcodeToFile140(M, OS, ShouldPreserveUseListOrder, Index,
                            EmitModuleHash);
      return false;
    }
    void getAnalysisUsage(AnalysisUsage &AU) const override {
      AU.setPreservesAll();
      if (EmitSummaryIndex)
        AU.addRequired<ModuleSummaryIndexWrapperPass>();
    }
  };
}

char WriteBitcodePass140::ID = 0;
INITIALIZE_PASS_BEGIN(WriteBitcodePass140, "write-bitcode140", "Write 14.0 Bitcode", false,
                      true)
INITIALIZE_PASS_DEPENDENCY(ModuleSummaryIndexWrapperPass)
INITIALIZE_PASS_END(WriteBitcodePass140, "write-bitcode140", "Write 14.0 Bitcode", false,
                    true)

ModulePass *llvm::createBitcodeWriterPass140(raw_ostream &Str,
                                             bool ShouldPreserveUseListOrder,
                                             bool EmitSummaryIndex, bool EmitModuleHash) {
  return new WriteBitcodePass140(Str, ShouldPreserveUseListOrder,
                              EmitSummaryIndex, EmitModuleHash);
}

bool llvm::isBitcodeWriterPass140(Pass *P) {
  return P->getPassID() == (llvm::AnalysisID)&WriteBitcodePass140::ID;
}
