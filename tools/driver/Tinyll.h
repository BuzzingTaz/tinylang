#ifndef TINYLL_H
#define TINYLL_H

#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Lexer/Lexer.h"
#include "tinylang/Parser/Parser.h"
#include "llvm/ADT/StringRef.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <system_error>

class Tinyll {
public:
  Tinyll() { moduleInit(); };

  void exec(llvm::SourceMgr &SrcMgr, DiagnosticsEngine &Diags) {

    auto TheLexer = Lexer(SrcMgr, Diags);
    auto TheSema = Sema(Diags);
    auto TheParser = Parser(TheLexer, TheSema);
    TheParser.parse();

    // Compile to LLVM IR

    /*Module->print(llvm::outs(), nullptr);*/
    /*// save to a .ll file*/
    /*saveModuleToFile("./out.ll");*/
    return;
  }

private:
  std::unique_ptr<llvm::LLVMContext> Ctx;
  std::unique_ptr<llvm::Module> Module;
  std::unique_ptr<llvm::IRBuilder<>> Builder;

  void moduleInit() {
    Ctx = std::make_unique<llvm::LLVMContext>();
    Module = std::make_unique<llvm::Module>("RacketLLVM", *Ctx);
  }

  void saveModuleToFile(llvm::StringRef FileName) {
    std::error_code ErrorCode;
    llvm::raw_fd_ostream OutLl(FileName, ErrorCode);
    Module->print(OutLl, nullptr);
  }
};
#endif
