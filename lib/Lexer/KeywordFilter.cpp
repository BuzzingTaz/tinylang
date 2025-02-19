#include "tinylang/Lexer/KeywordFilter.h"
#include "llvm/ADT/StringRef.h"

void KeywordFilter::addKeyword(llvm::StringRef Keyword,
                               tok::TokenKind TokenCode) {
  HashTable.insert(std::make_pair(Keyword, TokenCode));
}

void KeywordFilter::addKeywords() {
#define KEYWORD(NAME, FLAGS) addKeyword(llvm::StringRef(#NAME), tok::kw_##NAME);
#include "tinylang/Basic/TokenKinds.def"
}