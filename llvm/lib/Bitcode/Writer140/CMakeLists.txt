add_llvm_component_library(LLVMBitWriter140
  BitWriter140.cpp
  BitcodeWriter140.cpp
  BitcodeWriterPass140.cpp
  ValueEnumerator140.cpp

  DEPENDS
  intrinsics_gen

  LINK_COMPONENTS
  Analysis
  Core
  MC
  Object
  Support
  )
