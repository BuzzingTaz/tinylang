#include "Tinyll.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/raw_ostream.h>

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);

  llvm::SmallVector<const char *, 256> argv(argv_ + 1, argv_ + argc_);
  llvm::outs() << "Tinylang " << "0.0.1" << "\n";

  Tinyll Compiler;

  for (const char *F : argv) {
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr =
        llvm::MemoryBuffer::getFile(F);
    if (std::error_code BufferError = FileOrErr.getError()) {
      llvm::errs() << "Error reading " << F << ": " << BufferError.message()
                   << "\n";
      continue;
    }
    llvm::SourceMgr SrcMgr;
    DiagnosticsEngine Diags(SrcMgr);
    SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

    Compiler.exec(SrcMgr, Diags);
  }

  return 0;
}
