#ifndef TINYLANG_LEXER_LEXER_H
#define TINYLANG_LEXER_LEXER_H

#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/KeywordFilter.h"
#include "tinylang/Lexer/Token.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>
#include <llvm/Support/SourceMgr.h>

using namespace tinylang;

class Lexer {
  unsigned CurBuffer = 0;
  llvm::StringRef Buffer;
  const char *BufferPtr;
  llvm::SourceMgr &SrcMgr;

  KeywordFilter KWFilter;

  DiagnosticsEngine &Diags;

public:
  Lexer(llvm::SourceMgr &SrcMgr, DiagnosticsEngine &Diags)
      : SrcMgr(SrcMgr), Diags(Diags) {
    CurBuffer = SrcMgr.getMainFileID();
    Buffer = SrcMgr.getMemoryBuffer(CurBuffer)->getBuffer();
    BufferPtr = Buffer.begin();
    KWFilter.addKeywords();
  }

  DiagnosticsEngine &getDiagnostics() const { return Diags; }

  void next(Token &Result);

private:
  void identifier(Token &Result);
  void number(Token &Result);
  void string(Token &Result);
  void comment();
  void formToken(Token &Result, const char *TokEnd, tok::TokenKind Kind);

  llvm::SMLoc getLoc() { return llvm::SMLoc::getFromPointer(BufferPtr); }
};

#endif
