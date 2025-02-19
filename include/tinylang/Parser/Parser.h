#ifndef TINYLANG_PARSER_PARSER_H
#define TINYLANG_PARSER_PARSER_H

#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/Lexer.h"
#include "tinylang/Parser/NTStart.h"
#include "llvm/Support/raw_ostream.h"
#include <sys/types.h>
#include <unordered_set>

class Parser {
  Lexer &Lex;
  Token Tok;
  bool HasError;
  NTstart StartSet;

  void error() {
    llvm::errs() << "Error: unexpected token: " << Tok.getText() << '\n';
    HasError = true;
  }

  void advance() { Lex.next(Tok); }

  bool expect(tok::TokenKind Kind) {
    if (Tok.getKind() == Kind)
      return true;
    error();
    return false;
  }

  bool consume(tok::TokenKind Kind) {
    if (expect(Kind)) {
      advance();
      return true;
    }
    return false;
  }

  template <class... Tokens> bool skipUntil(Tokens... Toks) {
    std::unordered_set<tok::TokenKind> Skipset = {tok::eof, Toks...};
    while (true) {
      if (Skipset.count(Tok.getKind())) {
        if (Tok.getKind() == tok::eof)
          return false;
        return true;
      }
      advance();
    }
  }

  bool parseProgram();

  bool parseImport();

  bool parseBlock();

  bool parseDeclaration();
  bool parseConstantDeclaration();
  bool parseVariableDeclaration();
  bool parseProcedureDeclaration();
  bool parseFormalParameters();
  bool parseFormalParametersList();
  bool parseFormalParameter();

  bool parseStatementSequence();
  bool parseStatement();
  bool parseIfStatement();
  bool parseWhileStatement();
  bool parseExpList();
  bool parseExpression();
  bool parseRelation();
  bool parseSimpleExpression();
  bool parseAddOperator();
  bool parseTerm();
  bool parseMulOperator();
  bool parseFactor();
  bool parseIdentList();
  bool parseQualident();

public:
  Parser(Lexer &Lex) : Lex(Lex), HasError(false) { advance(); }

  bool hasError() { return HasError; }

  bool parse();
};
#endif
