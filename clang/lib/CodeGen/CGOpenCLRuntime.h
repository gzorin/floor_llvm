//===----- CGOpenCLRuntime.h - Interface to OpenCL Runtimes -----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This provides an abstract class for OpenCL code generation.  Concrete
// subclasses of this implement code generation for specific OpenCL
// runtime libraries.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_CODEGEN_CGOPENCLRUNTIME_H
#define LLVM_CLANG_LIB_CODEGEN_CGOPENCLRUNTIME_H

#include "clang/AST/Expr.h"
#include "clang/AST/Type.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

namespace clang {

class BlockExpr;
class Expr;
class VarDecl;

namespace CodeGen {

class CodeGenFunction;
class CodeGenModule;

class CGOpenCLRuntime {
protected:
  CodeGenModule &CGM;
  llvm::Type *PipeROTy;
  llvm::Type *PipeWOTy;
  llvm::PointerType *SamplerTy;

  /// Structure for enqueued block information.
  struct EnqueuedBlockInfo {
    llvm::Function *InvokeFunc; /// Block invoke function.
    llvm::Function *Kernel;     /// Enqueued block kernel.
    llvm::Value *BlockArg;      /// The first argument to enqueued block kernel.
  };
  /// Maps block expression to block information.
  llvm::DenseMap<const Expr *, EnqueuedBlockInfo> EnqueuedBlockMap;

  virtual llvm::Type *getPipeType(const PipeType *T, StringRef Name,
                                  llvm::Type *&PipeTy);

public:
  CGOpenCLRuntime(CodeGenModule &CGM) : CGM(CGM),
    PipeROTy(nullptr), PipeWOTy(nullptr), SamplerTy(nullptr) {}
  virtual ~CGOpenCLRuntime();

  /// Emit the IR required for a work-group-local variable declaration, and add
  /// an entry to CGF's LocalDeclMap for D.  The base class does this using
  /// CodeGenFunction::EmitStaticVarDecl to emit an internal global for D.
  virtual void EmitWorkGroupLocalVarDecl(CodeGenFunction &CGF,
                                         const VarDecl &D);

  virtual llvm::Type *convertOpenCLSpecificType(const Type *T);

  virtual llvm::Type *getPipeType(const PipeType *T);

  llvm::PointerType *getSamplerType(const Type *T);

  // Returns a value which indicates the size in bytes of the pipe
  // element.
  virtual llvm::Value *getPipeElemSize(const Expr *PipeArg);

  // Returns a value which indicates the alignment in bytes of the pipe
  // element.
  virtual llvm::Value *getPipeElemAlign(const Expr *PipeArg);

  /// \return __generic void* type.
  llvm::PointerType *getGenericVoidPointerType();

  /// \return enqueued block information for enqueued block.
  EnqueuedBlockInfo emitOpenCLEnqueuedBlock(CodeGenFunction &CGF,
                                            const Expr *E);

  /// Record invoke function and block literal emitted during normal
  /// codegen for a block expression. The information is used by
  /// emitOpenCLEnqueuedBlock to emit wrapper kernel.
  ///
  /// \param InvokeF invoke function emitted for the block expression.
  /// \param Block block literal emitted for the block expression.
  void recordBlockInfo(const BlockExpr *E, llvm::Function *InvokeF,
                       llvm::Value *Block);

  /// \return LLVM block invoke function emitted for an expression derived from
  /// the block expression.
  llvm::Function *getInvokeFunction(const Expr *E);
};

class Ocl20Mangler {
public:
  Ocl20Mangler(llvm::SmallVectorImpl<char>&);

  // \brief Appends the mangled representation of reserve_id_t parameter to the
  //  mangled string.
  Ocl20Mangler& appendReservedId();

  // \brief Appends the mangled representation of pipe_t parameter to the
  //  mangled string.
  Ocl20Mangler& appendPipe();

  // \brief Appends the mangled representation of 'int' parameter to the
  //  mangled string.
  Ocl20Mangler& appendInt();

  // \brief Appends the mangled representation of 'unsigned int' parameter to the
  // mangled string.
  Ocl20Mangler& appendUint();

  // \brief Appends the mangled representation of a pointer.
  Ocl20Mangler& appendPointer();

  // \brief Appends the mangled representation of void.
  Ocl20Mangler& appendVoid();

  // \brief Appends the mangled representation of a pointer with a given address
  // space.
  // \param addressSapace The address space of the pointer. Valid values are
  // [0,4].
  Ocl20Mangler& appendPointer(int addressSapace);

private:

  // \brief Appends the given string to the mangled prototype.
  Ocl20Mangler& appendString(llvm::StringRef);

  llvm::SmallVectorImpl<char> *MangledString;
};

}
}

#endif
