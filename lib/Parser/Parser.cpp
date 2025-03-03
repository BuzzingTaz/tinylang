#include "tinylang/Parser/Parser.h"
#include "tinylang/AST/AST.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Sema/Sema.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/SMLoc.h>
#include <llvm/Support/raw_ostream.h>

using namespace tinylang;
namespace {
OpInfo fromTok(Token Tok) { return OpInfo(Tok.getLocation(), Tok.getKind()); }
} // namespace

DeclModule *Parser::parse() {
  DeclModule *D = nullptr;
  bool Res = parseProgram(D);
  expect(tok::eof);
  return D;
}

bool Parser::parseProgram(DeclModule *&D) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!consume(tok::kw_MODULE))
    return ErrorHandler();

  if (!expect(tok::identifier))
    return ErrorHandler();
  Ident *Id = new Ident(Tok.getLocation(), Tok.getIdentifier());
  Actions.actOnModuleDeclaration(D, Id);

  EnterDeclScope S(Actions, D);
  advance();

  if (!consume(tok::semicolon))
    return ErrorHandler();

  while (Tok.isOneOf(StartSet.Import)) {
    if (!parseImport())
      return ErrorHandler();
  }

  DeclList Decls;
  StatSeq Stats;
  if (!parseBlock(Decls, Stats))
    return ErrorHandler();

  if (!expect(tok::identifier))
    return ErrorHandler();

  Ident *Id2 = new Ident(Tok.getLocation(), Tok.getIdentifier());
  Actions.actOnModuleDeclaration(D, Id2, Decls, Stats);
  advance();

  if (!consume(tok::period))
    return ErrorHandler();

  return true;
}

bool Parser::parseImport() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  IdentList ImportIds;
  Ident *ModuleId;

  if (Tok.is(tok::kw_FROM)) {
    if (!expect(tok::identifier))
      return ErrorHandler();
    ModuleId = new Ident(Tok.getLocation(), Tok.getIdentifier());
    advance();
  }

  if (!consume(tok::kw_IMPORT))
    return ErrorHandler();

  if (!parseIdentList(ImportIds))
    return ErrorHandler();

  if (!expect(tok::semicolon))
    return ErrorHandler();
  Actions.actOnImport(ModuleId, ImportIds);

  return true;
}

bool Parser::parseBlock(DeclList &Decls, StatSeq &Stats) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  while (Tok.isOneOf(StartSet.Declaration)) {
    if (!parseDeclaration(Decls))
      return ErrorHandler();
  }

  if (Tok.is(tok::kw_BEGIN)) {
    advance();
    if (!parseStatementSequence(Stats))
      return ErrorHandler();
  }

  if (!expect(tok::kw_END))
    return ErrorHandler();

  return true;
}

bool Parser::parseDeclaration(DeclList &Decls) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (Tok.is(tok::kw_CONST)) {
    advance();
    while (Tok.isOneOf(StartSet.ConstantDeclaration)) {
      if (!parseConstantDeclaration(Decls))
        return ErrorHandler();
      if (!consume(tok::semicolon))
        return ErrorHandler();
    }
  } else if (Tok.is(tok::kw_VAR)) {
    advance();
    while (Tok.isOneOf(StartSet.VariableDeclaration)) {
      if (!parseVariableDeclaration(Decls))
        ErrorHandler();
      if (!consume(tok::semicolon))
        return ErrorHandler();
    }
  } else if (Tok.is(tok::kw_PROCEDURE)) {
    if (!parseProcedureDeclaration(Decls))
      return ErrorHandler();

    if (!consume(tok::semicolon))
      return ErrorHandler();
  } else
    return ErrorHandler();
  return true;
}

bool Parser::parseConstantDeclaration(DeclList &Decls) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  Expr *E = nullptr;
  if (!expect(tok::identifier))
    return ErrorHandler();
  Ident *Id = new Ident(Tok.getLocation(), Tok.getIdentifier());

  if (!consume(tok::equals))
    return ErrorHandler();

  if (!parseExpression(E))
    return ErrorHandler();

  Actions.actOnConstantDeclaration(Decls, Id, E);
  return true;
}

bool Parser::parseVariableDeclaration(DeclList &Decls) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  Decl *D = nullptr;
  IdentList Ids;

  if (!parseIdentList(Ids))
    return ErrorHandler();

  if (!consume(tok::colon))
    return ErrorHandler();

  if (!parseQualident(D))
    return ErrorHandler();

  Actions.actOnVariableDeclaration(Decls, Ids, D);
  return true;
}

bool Parser::parseProcedureDeclaration(DeclList &ParentDecls) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!consume(tok::kw_PROCEDURE))
    return ErrorHandler();

  if (!expect(tok::identifier))
    return ErrorHandler();

  DeclProc *D = nullptr;
  Ident *Id = new Ident(Tok.getLocation(), Tok.getIdentifier());
  Actions.actOnProcedureDeclaration(D, Id);
  EnterDeclScope S(Actions, D);

  FormParList Params;
  Decl *RetType = nullptr;

  advance();
  if (Tok.isOneOf(StartSet.FormalParameters)) {
    if (!parseFormalParameters(Params, RetType))
      return ErrorHandler();
  }

  Actions.actOnProcedureHeading(D, Params, RetType);

  if (!consume(tok::semicolon))
    return ErrorHandler();

  DeclList Decls;
  StatSeq Stats;
  if (!parseBlock(Decls, Stats))
    return ErrorHandler();

  if (!expect(tok::identifier))
    return ErrorHandler();

  Actions.actOnProcedureDeclaration(D, Id, Params, RetType, Decls, Stats);
  ParentDecls.push_back(D);
  advance();

  if (!consume(tok::semicolon))
    return ErrorHandler();

  return true;
}

bool Parser::parseFormalParameters(FormParList &Params, Decl *&RetType) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!consume(tok::l_paren))
    return ErrorHandler();

  if (Tok.isOneOf(StartSet.FormalParameterList)) {
    if (!parseFormalParametersList(Params))
      return ErrorHandler();

    if (!consume(tok::r_paren))
      return ErrorHandler();
  }

  if (Tok.is(tok::colon)) {
    advance();
    if (!parseQualident(RetType))
      return ErrorHandler();
  }

  return true;
}

bool Parser::parseFormalParametersList(FormParList &Params) {
  auto ErrorHandler = [this]() { return skipUntil(); };
  if (!parseFormalParameter(Params))
    return ErrorHandler();

  while (Tok.is(tok::semicolon)) {
    advance();
    if (!parseFormalParameter(Params))
      return ErrorHandler();
  }

  return true;
}

bool Parser::parseFormalParameter(FormParList &Params) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  IdentList Ids;
  Decl *D;
  bool IsVar = false;

  if (Tok.is(tok::kw_VAR)) {
    IsVar = true;
    advance();
  }

  if (!parseIdentList(Ids))
    return ErrorHandler();

  if (!consume(tok::colon))
    return ErrorHandler();

  if (!parseQualident(D))
    return ErrorHandler();

  Actions.actOnFormalParameterDeclaration(Params, Ids, D, IsVar);

  return true;
}

bool Parser::parseStatementSequence(StatSeq &Stats) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!parseStatement(Stats))
    return ErrorHandler();

  while (Tok.is(tok::semicolon)) {
    advance();
    if (!parseStatement(Stats))
      return ErrorHandler();
  }

  return true;
}

bool Parser::parseStatement(StatSeq &Stats) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (Tok.isOneOf(StartSet.Qualident)) {
    Decl *D;
    Expr *E = nullptr;
    llvm::SMLoc Loc = Tok.getLocation();
    if (!parseQualident(D))
      return ErrorHandler();
    if (Tok.is(tok::defines)) {
      advance();
      if (!parseExpression(E))
        return ErrorHandler();
      Actions.actOnAssignment(Stats, Loc, D, E);
    } else if (Tok.is(tok::l_paren)) {
      ExprList Exprs;
      advance();
      if (Tok.isOneOf(StartSet.ExpList))
        if (!parseExpList(Exprs))
          return ErrorHandler();
      if (!consume(tok::r_paren))
        return ErrorHandler();
      Actions.actOnProcCall(Stats, Loc, D, Exprs);
    }
  } else if (Tok.isOneOf(StartSet.IfStatement)) {
    if (!parseIfStatement(Stats))
      return ErrorHandler();
  } else if (Tok.isOneOf(StartSet.WhileStatement)) {
    if (!parseWhileStatement(Stats))
      return ErrorHandler();
  } else if (Tok.is(tok::kw_RETURN)) {
    if (!parseReturnStatement(Stats))
      return ErrorHandler();
  }

  else {
    return ErrorHandler();
  }

  return true;
}

bool Parser::parseIfStatement(StatSeq &Stats) {
  auto ErrorHandler = [this]() {
    return skipUntil(tok::semicolon, tok::kw_ELSE, tok::kw_END);
  };

  Expr *E = nullptr;
  StatSeq Then, Else;
  llvm::SMLoc Loc = Tok.getLocation();
  if (!consume(tok::kw_IF))
    return ErrorHandler();

  if (!parseExpression(E))
    return ErrorHandler();

  if (!consume(tok::kw_THEN))
    return ErrorHandler();

  if (!parseStatementSequence(Then))
    return ErrorHandler();

  if (Tok.is(tok::kw_ELSE)) {
    advance();
    if (!parseStatementSequence(Else))
      return ErrorHandler();
  }
  if (!expect(tok::kw_END))
    return ErrorHandler();
  Actions.actOnIfStatement(Stats, Loc, E, Then, Else);
  advance();

  return true;
}

bool Parser::parseWhileStatement(StatSeq &Stats) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  Expr *E = nullptr;
  StatSeq LoopStats;
  llvm::SMLoc Loc = Tok.getLocation();
  if (!consume(tok::kw_WHILE))
    return ErrorHandler();

  if (!parseExpression(E))
    return ErrorHandler();

  if (!consume(tok::kw_DO))
    return ErrorHandler();

  if (!parseStatementSequence(LoopStats))
    return ErrorHandler();

  if (!consume(tok::kw_END))
    return ErrorHandler();
  Actions.actOnWhileStatement(Stats, Loc, E, LoopStats);
  advance();

  return true;
}

bool Parser::parseReturnStatement(StatSeq &Stmts) {
  auto Errorhandler = [this]() { return skipUntil(); };

  Expr *E = nullptr;
  llvm::SMLoc Loc = Tok.getLocation();
  if (consume(tok::kw_RETURN))
    return Errorhandler();

  if (Tok.isOneOf(StartSet.Expression)) {
    if (parseExpression(E))
      return Errorhandler();
  }
  Actions.actOnReturnStatement(Stmts, Loc, E);
  return false;
}

bool Parser::parseExpList(ExprList &Exprs) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  Expr *E = nullptr;
  if (!parseExpression(E))
    return ErrorHandler();

  if (E)
    Exprs.push_back(E);
  while (Tok.is(tok::comma)) {
    E = nullptr;
    advance();

    if (!parseExpression(E))
      return ErrorHandler();
    if (E)
      Exprs.push_back(E);
  }

  return true;
}

bool Parser::parseExpression(Expr *&E) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!parseSimpleExpression(E))
    return ErrorHandler();

  if (Tok.isOneOf(StartSet.Relation)) {
    OpInfo Op;
    Expr *Right = nullptr;

    if (!parseRelation(Op))
      return ErrorHandler();

    if (!parseSimpleExpression(Right))
      return ErrorHandler();
    Actions.actOnExpression(E, E, Right, Op);
  }

  return true;
}

bool Parser::parseRelation(OpInfo &Op) {
  auto ErrorHandler = [this]() { return skipUntil(); };
  if (Tok.isOneOf(StartSet.Relation)) {
    for (auto &Kind : StartSet.Relation) {
      if (Tok.is(Kind)) {
        Op = fromTok(Tok);
        advance();
        break;
      }
    }
    return true;
  }
  return ErrorHandler();
}

bool Parser::parseSimpleExpression(Expr *&E) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  OpInfo PrefixOp;
  if (Tok.isOneOf(tok::plus, tok::minus)) {
    PrefixOp = fromTok(Tok);
    advance();
  }

  if (!parseTerm(E))
    return ErrorHandler();

  while (Tok.isOneOf(StartSet.Term)) {
    OpInfo Op;
    Expr *Right = nullptr;
    if (!parseAddOperator(Op))
      return ErrorHandler();

    if (!parseTerm(Right))
      return ErrorHandler();
    Actions.actOnSimpleExpression(E, E, Right, Op);
  }
  if (!PrefixOp.isUnspecified())
    Actions.actOnPrefixExpression(E, E, PrefixOp);

  return true;
}

bool Parser::parseAddOperator(OpInfo &Op) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (Tok.isOneOf(StartSet.AddOperator)) {
    for (auto &Kind : StartSet.AddOperator) {
      if (Tok.is(Kind)) {
        Op = fromTok(Tok);
        advance();
        break;
      }
    }
    return true;
  }
  return ErrorHandler();
}

bool Parser::parseTerm(Expr *&E) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!parseFactor(E))
    return ErrorHandler();

  while (Tok.isOneOf(StartSet.MulOperator)) {
    OpInfo Op;
    Expr *Right = nullptr;
    if (!parseMulOperator(Op))
      return ErrorHandler();

    if (!parseFactor(Right))
      return ErrorHandler();
    Actions.actOnTerm(E, E, Right, Op);
  }
  return true;
}

bool Parser::parseMulOperator(OpInfo &Op) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (Tok.isOneOf(StartSet.MulOperator)) {
    for (auto &Kind : StartSet.MulOperator) {
      if (Tok.is(Kind)) {
        Op = fromTok(Tok);
        advance();
        break;
      }
    }
    return true;
  }
  return ErrorHandler();
}

bool Parser::parseFactor(Expr *&E) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  // integer_literal
  if (Tok.is(tok::integer_literal)) {
    Actions.actOnIntegerLiteral(E, Tok.getLocation(), Tok.getLiteralData());
    advance();
  }
  // "(" expression ")"
  else if (Tok.is(tok::l_paren)) {
    advance();
    if (!parseExpression(E))
      return ErrorHandler();
    if (!consume(tok::r_paren))
      return ErrorHandler();
  }
  // "NOT" factor
  else if (Tok.is(tok::kw_NOT)) {
    OpInfo Op = fromTok(Tok);
    advance();
    if (!parseFactor(E))
      return ErrorHandler();
    Actions.actOnPrefixExpression(E, E, Op);
  }
  // qualident ( "(" (explist)? ")" )?
  else if (Tok.is(tok::identifier)) {
    Decl *D;
    ExprList Exprs;
    if (!parseQualident(D))
      return ErrorHandler();

    if (Tok.is(tok::l_paren)) {
      advance();

      if (Tok.isOneOf(StartSet.ExpList)) {
        if (!parseExpList(Exprs))
          return ErrorHandler();
      }

      if (!consume(tok::r_paren))
        return ErrorHandler();
    } else if /* case where ident */ (Tok.isOneOf(
        tok::hash, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus,
        tok::slash, tok::semicolon, tok::less, tok::lesseq, tok::equals,
        tok::great, tok::greateq, tok::kw_AND, tok::kw_DIV, tok::kw_DO,
        tok::kw_ELSE, tok::kw_END, tok::kw_MOD, tok::kw_OR, tok::kw_THEN)) {
      Actions.actOnVariable(E, E, D);
    }
  }
  return true;
}

bool Parser::parseQualident(Decl *&D) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  D = nullptr;
  if (!expect(tok::identifier))
    return ErrorHandler();
  Ident *Id = new Ident(Tok.getLocation(), Tok.getIdentifier());
  Actions.actOnQualidentPart(D, Id);
  advance();

  while (Tok.is(tok::period) && (llvm::isa<DeclModule>(D))) {
    advance();
    if (!expect(tok::identifier))
      return ErrorHandler();
    Actions.actOnQualidentPart(D, Id);
    advance();
  }

  return true;
}

bool Parser::parseIdentList(IdentList &Ids) {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!expect(tok::identifier))
    return ErrorHandler();
  Ids.push_back(new Ident(Tok.getLocation(), Tok.getIdentifier()));
  advance();

  while (Tok.is(tok::comma)) {
    advance();
    if (!expect(tok::identifier))
      return ErrorHandler();
    Ids.push_back(new Ident(Tok.getLocation(), Tok.getIdentifier()));
    advance();
  }

  return true;
}
