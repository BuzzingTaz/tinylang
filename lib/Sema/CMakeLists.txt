
set(LLVM_LINK_COMPONENTS support)

add_tinylang_library(tinylangSema
  Sema.cpp

  LINK_LIBS
  tinylangLexer
  )