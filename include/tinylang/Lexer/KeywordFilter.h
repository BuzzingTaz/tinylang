#ifndef TINYLANG_LEXER_KEYWORDFILTER_H
#define TINYLANG_LEXER_KEYWORDFILTER_H

#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/Token.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"

using namespace tinylang;

class KeywordFilter {
  llvm::StringMap<tok::TokenKind> HashTable;
  void addKeyword(llvm::StringRef Keyword, tok::TokenKind TokenCode);

public:
  void addKeywords();

  tok::TokenKind getKeyword(llvm::StringRef Name,
                            tok::TokenKind DefaultTokenCode = tok::unknown) {
    auto Result = HashTable.find(Name);
    if (Result != HashTable.end())
      return Result->second;
    return DefaultTokenCode;
  }
};

#endif
