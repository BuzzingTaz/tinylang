#ifndef TINYLANG_PARSER_PARSER_H
#define TINYLANG_PARSER_PARSER_H

#include "tinylang/AST/AST.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/Lexer.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/Support/raw_ostream.h"
#include <sys/types.h>
#include <unordered_set>
#include <utility>

class Parser {
  Lexer &Lex;
  Token Tok;
  bool HasError;

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
    const int NArgs = sizeof...(Toks) + 1;
    llvm::SmallSet<tok::TokenKind, NArgs> Skipset = {
        tok::eof, (std::forward<Tokens>(Toks))...};
    while (true) {
      if (Skipset.contains(Tok)) {
        if (Tok.getKind() == tok::eof)
          return false;
        return true;
      }
      advance();
    }
  }

  AST *parseProgram();

  void parseImport();

  void parseBlock();

  void parseDeclaration();
  void parseConstantDeclaration();
  void parseVariableDeclaration();
  void parseProcedureDeclaration();
  void parseFormalParameters();
  void parseFormalParametersList();
  void parseFormalParameter();

  bool parseStatementSequence();
  void parseStatement();
  bool parseIfStatement();
  void parseWhileStatement();
  void parseExpList();
  bool parseExpression();
  void parseRelation();
  void parseSimpleExpression();
  void parseAdd();
  void parseTerm();
  void parseMulOperator();
  void parseFactor();
  void parseIdentList();
  void parseQualident();

public:
  Parser(Lexer &Lex) : Lex(Lex), HasError(false) { advance(); }

  bool hasError() { return HasError; }

  AST *parse();
};
#endif