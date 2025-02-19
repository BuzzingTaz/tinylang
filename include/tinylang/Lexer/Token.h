#ifndef TINYLANG_LEXER_TOKEN_H
#define TINYLANG_LEXER_TOKEN_H

#include "tinylang/Basic/TokenKinds.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
using namespace tinylang;

class Token {
  friend class Lexer;

private:
  llvm::StringRef Text;
  tok::TokenKind Kind;

public:
  tok::TokenKind getKind() const { return Kind; }
  llvm::StringRef getText() const { return Text; }
  size_t getLength() const { return Text.size(); }

  llvm::SMLoc getLocation() const {
    return llvm::SMLoc::getFromPointer(Text.begin());
  }

  llvm::StringRef getIdentifier() {
    assert(is(tok::identifier) && "Cannot get identifier of non-identifier");
    return Text;
  }
  llvm::StringRef getLiteralData() {
    assert(isOneOf(tok::integer_literal, tok::string_literal) &&
           "Cannot get literal data of non-literal");
    return Text;
  }

  bool is(tok::TokenKind K) const { return Kind == K; }
  bool isOneOf(tok::TokenKind K1, tok::TokenKind K2) const {
    return is(K1) || is(K2);
  }
  template <typename... Ts>
  bool isOneOf(tok::TokenKind K1, tok::TokenKind K2, Ts... Ks) const {
    return is(K1) || isOneOf(K2, Ks...);
  }
};

#endif