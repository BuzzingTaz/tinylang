#ifndef TINYLANG_LEXER_LEXER_H
#define TINYLANG_LEXER_LEXER_H

#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/KeywordFilter.h"
#include "tinylang/Lexer/Token.h"

using namespace tinylang;

class Lexer {
  const char *BufferStart;
  const char *BufferPtr;
  KeywordFilter KWFilter;

public:
  Lexer(const llvm::StringRef &Buffer) {
    BufferStart = Buffer.data();
    BufferPtr = BufferStart;
    KWFilter.addKeywords();
  }

  void next(Token &Tok);

private:
  void formToken(Token &Result, const char *TokEnd, tok::TokenKind Kind);
};

#endif