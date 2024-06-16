//===-- BitWriter140.cpp --------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm-c/BitWriter.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;


/*===-- Operations on modules ---------------------------------------------===*/

int LLVMWriteBitcodeToFile140(LLVMModuleRef M, const char *Path) {
  std::error_code EC;
  raw_fd_ostream OS(Path, EC, sys::fs::OF_None);

  if (EC)
    return -1;

  WriteBitcodeToFile140(*unwrap(M), OS);
  return 0;
}

int LLVMWriteBitcodeToFD140(LLVMModuleRef M, int FD, int ShouldClose,
                         int Unbuffered) {
  raw_fd_ostream OS(FD, ShouldClose, Unbuffered);

  WriteBitcodeToFile140(*unwrap(M), OS);
  return 0;
}

int LLVMWriteBitcodeToFileHandle140(LLVMModuleRef M, int FileHandle) {
  return LLVMWriteBitcodeToFD140(M, FileHandle, true, false);
}

LLVMMemoryBufferRef LLVMWriteBitcodeToMemoryBuffer140(LLVMModuleRef M) {
  std::string Data;
  raw_string_ostream OS(Data);

  WriteBitcodeToFile140(*unwrap(M), OS);
  return wrap(MemoryBuffer::getMemBufferCopy(OS.str()).release());
}
