#include "tinylang/Lexer/Lexer.h"

namespace charinfo {
LLVM_READNONE inline bool isWhitespace(char c) {
  return c == ' ' || c == '\t' || c == '\f' || c == '\v' || c == '\r' ||
         c == '\n';
}
LLVM_READNONE inline bool isDigit(char c) { return c >= '0' && c <= '9'; }
LLVM_READNONE inline bool isLetter(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}
} // namespace charinfo

void Lexer::next(Token &token) {
  while (*BufferPtr && charinfo::isWhitespace(*BufferPtr))
    ++BufferPtr;

  if (!*BufferPtr) {
    token.Kind = tok::TokenKind::eof;
    return;
  }

  // TODO: Take care of hexadecimal later
  if (charinfo::isLetter(*BufferPtr)) {
    const char *End = BufferPtr + 1;
    while (charinfo::isLetter(*End) or charinfo::isDigit(*End) or (*End) == '_')
      ++End;
    llvm::StringRef Name(BufferPtr, End - BufferPtr);
    if (KWFilter.getKeyword(Name) != tok::TokenKind::unknown) {
      formToken(token, End, KWFilter.getKeyword(Name));
    } else {
      formToken(token, End, tok::TokenKind::identifier);
    }
    return;
  }
  if (charinfo::isDigit(*BufferPtr)) {
    const char *End = BufferPtr + 1;
    while (charinfo::isDigit(*End))
      ++End;
    formToken(token, End, tok::TokenKind::integer_literal);
    return;
  }
  switch (*BufferPtr) {
  case '<':
    if (*(BufferPtr + 1) == '=')
      formToken(token, BufferPtr + 2, tok::lesseq);
    else
      formToken(token, BufferPtr + 1, tok::less);
    break;
  case '>':
    if (*(BufferPtr + 1) == '=')
      formToken(token, BufferPtr + 2, tok::greateq);
    else
      formToken(token, BufferPtr + 1, tok::great);
    break;
  case ':':
    if (*(BufferPtr + 1) == '=')
      formToken(token, BufferPtr + 2, tok::defines);
    else
      formToken(token, BufferPtr + 1, tok::colon);
    break;
#define CASE(ch, tok)                                                          \
  case ch:                                                                     \
    formToken(token, BufferPtr + 1, tok);                                      \
    break;
    CASE('(', tok::TokenKind::l_paren);
    CASE(')', tok::TokenKind::r_paren);
    CASE(';', tok::TokenKind::semicolon);
    CASE('.', tok::TokenKind::period);
    CASE(',', tok::TokenKind::comma);
    CASE('=', tok::TokenKind::equals);
    CASE('+', tok::TokenKind::plus);
    CASE('-', tok::TokenKind::minus);
    CASE('*', tok::TokenKind::star);
    CASE('/', tok::TokenKind::slash);
    CASE('#', tok::TokenKind::hash);
#undef CASE

  default:
    formToken(token, BufferPtr + 1, tok::TokenKind::unknown);
    break;
  }
  return;
}

void Lexer::formToken(Token &Tok, const char *TokEnd, tok::TokenKind Kind) {
  Tok.Kind = Kind;
  Tok.Text = llvm::StringRef(BufferPtr, TokEnd - BufferPtr);
  BufferPtr = TokEnd;
}
