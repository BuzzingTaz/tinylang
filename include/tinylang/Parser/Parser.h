#ifndef TINYLANG_PARSER_PARSER_H
#define TINYLANG_PARSER_PARSER_H

#include "tinylang/AST/AST.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/Lexer.h"
#include "tinylang/Parser/NTStart.h"
#include "tinylang/Sema/Sema.h"
#include "llvm/Support/raw_ostream.h"
#include <sys/types.h>
#include <unordered_set>

class Parser {
  Lexer &Lex;
  Token Tok;
  NTstart StartSet;
  Sema &Actions;

  void error() {
    llvm::errs() << "Error: unexpected token: " << Tok.getText() << '\n';
  }
  DiagnosticsEngine &getDiagnostics() const { return Lex.getDiagnostics(); }

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

  bool parseProgram(DeclModule *&D);

  bool parseImport();

  bool parseBlock(DeclList &Decls, StatSeq &Stats);

  bool parseDeclaration(DeclList &Decls);
  bool parseConstantDeclaration(DeclList &Decls);
  bool parseVariableDeclaration(DeclList &Decls);
  bool parseProcedureDeclaration(DeclList &ParentDecls);
  bool parseFormalParameters(FormParList &Params, Decl *&RetType);
  bool parseFormalParametersList(FormParList &Params);
  bool parseFormalParameter(FormParList &Params);

  bool parseStatementSequence(StatSeq &Stats);
  bool parseStatement(StatSeq &Stats);
  bool parseIfStatement(StatSeq &Stats);
  bool parseWhileStatement(StatSeq &Stats);
  bool parseReturnStatement(StatSeq &Stats);
  bool parseExpList(ExprList &Exprs);
  bool parseExpression(Expr *&E);
  bool parseRelation(OpInfo &Op);
  bool parseSimpleExpression(Expr *&E);
  bool parseAddOperator(OpInfo &Op);
  bool parseTerm(Expr *&E);
  bool parseMulOperator(OpInfo &Op);
  bool parseFactor(Expr *&E);
  bool parseIdentList(IdentList &Ids);
  bool parseQualident(Decl *&Decl);

public:
  Parser(Lexer &Lex, Sema &Actions) : Lex(Lex), Actions(Actions) { advance(); };

  DeclModule *parse();
};
#endif
