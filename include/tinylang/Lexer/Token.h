#ifndef TINYLANG_LEXER_TOKEN_H
#define TINYLANG_LEXER_TOKEN_H

#include "tinylang/Basic/TokenKinds.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include <unordered_set>
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
  bool is(std::unordered_set<tok::TokenKind> K) const { return K.count(Kind); }

  template <typename... TSets> bool isOneOf(TSets... KSets) {
    return (... || is(KSets));
  }
};

#endif