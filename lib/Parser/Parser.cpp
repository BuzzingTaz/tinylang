#include "tinylang/Parser/Parser.h"
#include "tinylang/Basic/TokenKinds.h"

using namespace tinylang;

bool Parser::parse() {
  bool Res = parseProgram();
  expect(tok::eof);
  return Res;
}

bool Parser::parseProgram() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!consume(tok::kw_MODULE))
    return ErrorHandler();

  if (!consume(tok::identifier))
    return ErrorHandler();

  if (!consume(tok::semicolon))
    return ErrorHandler();

  while (Tok.isOneOf(tok::kw_FROM, tok::kw_IMPORT)) {
    if (!parseImport())
      return ErrorHandler();
  }

  if (!parseBlock())
    return ErrorHandler();

  if (!consume(tok::identifier))
    return ErrorHandler();

  if (!consume(tok::period))
    return ErrorHandler();

  return true;
}

bool Parser::parseImport() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (Tok.is(tok::kw_FROM))
    if (!consume(tok::identifier))
      return ErrorHandler();

  if (!consume(tok::kw_IMPORT))
    return ErrorHandler();

  if (!parseIdentList())
    return ErrorHandler();

  if (!expect(tok::semicolon))
    return ErrorHandler();

  return true;
}

bool Parser::parseBlock() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  while (Tok.isOneOf(StartSet.Declaration)) {
    if (!parseDeclaration())
      return ErrorHandler();
  }

  if (Tok.is(tok::kw_BEGIN)) {
    advance();
    if (!parseStatementSequence())
      return ErrorHandler();
  }

  if (!expect(tok::kw_END))
    return ErrorHandler();

  return true;
}

bool Parser::parseDeclaration() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (Tok.is(tok::kw_CONST))
    while (Tok.isOneOf(StartSet.ConstantDeclaration)) {
      if (!parseConstantDeclaration())
        return ErrorHandler();
      if (!consume(tok::semicolon))
        return ErrorHandler();
    }
  else if (Tok.is(tok::kw_VAR))
    while (Tok.isOneOf(StartSet.VariableDeclaration)) {
      if (!parseVariableDeclaration())
        ErrorHandler();
      if (!consume(tok::semicolon))
        return ErrorHandler();
    }
  else {
    if (!parseProcedureDeclaration())
      return ErrorHandler();

    if (!consume(tok::semicolon))
      return ErrorHandler();
  }
  return true;
}
bool Parser::parseConstantDeclaration() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!consume(tok::identifier))
    return ErrorHandler();

  if (!consume(tok::equals))
    return ErrorHandler();

  if (!parseExpression())
    return ErrorHandler();

  return true;
}
bool Parser::parseVariableDeclaration() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!parseIdentList())
    return ErrorHandler();

  if (!consume(tok::equals))
    return ErrorHandler();

  if (!parseQualident())
    return ErrorHandler();
  return true;
}

bool Parser::parseProcedureDeclaration() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!consume(tok::kw_PROCEDURE))
    return ErrorHandler();

  if (!consume(tok::identifier))
    return ErrorHandler();

  if (Tok.isOneOf(StartSet.FormalParameters)) {
    if (!parseFormalParameters())
      return ErrorHandler();
  }

  return true;
}

bool Parser::parseFormalParameters() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!consume(tok::l_paren))
    return ErrorHandler();

  if (Tok.isOneOf(StartSet.FormalParameterList)) {
    if (!parseFormalParametersList())
      return ErrorHandler();

    if (!consume(tok::r_paren))
      return ErrorHandler();
  }

  if (Tok.is(tok::colon)) {
    if (!parseQualident())
      return ErrorHandler();
  }

  return true;
}

bool Parser::parseFormalParametersList() {
  auto ErrorHandler = [this]() { return skipUntil(); };
  if (!parseFormalParameter())
    return ErrorHandler();

  while (Tok.is(tok::semicolon)) {
    advance();
    if (!parseFormalParameter())
      return ErrorHandler();
  }

  return true;
}

bool Parser::parseFormalParameter() {
  auto ErrorHandler = [this]() { return skipUntil(); };
  if (Tok.is(tok::kw_VAR))
    advance();

  if (!parseIdentList())
    return ErrorHandler();

  if (!consume(tok::colon))
    return ErrorHandler();

  if (!parseQualident())
    return ErrorHandler();

  return true;
}

bool Parser::parseStatementSequence() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!parseStatement())
    return ErrorHandler();

  while (Tok.is(tok::semicolon)) {
    advance();
    if (!parseStatement())
      return ErrorHandler();
  }

  return true;
}

bool Parser::parseStatement() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (Tok.isOneOf(StartSet.Qualident)) {
    if (!parseQualident())
      return ErrorHandler();
    if (Tok.is(tok::defines)) {
      advance();
      if (!parseExpression())
        return ErrorHandler();
    } else if (Tok.is(tok::l_paren)) {
      advance();
      if (Tok.isOneOf(StartSet.ExpList))
        if (!parseExpList())
          return ErrorHandler();
      if (!consume(tok::r_paren))
        return ErrorHandler();
    } else {
      return true;
    }
  } else if (Tok.isOneOf(StartSet.IfStatement)) {
    if (!parseIfStatement())
      return ErrorHandler();
  } else if (Tok.isOneOf(StartSet.WhileStatement)) {
    if (!parseWhileStatement())
      return ErrorHandler();
  } else if (Tok.is(tok::kw_RETURN)) {
    advance();
    if (Tok.isOneOf(StartSet.Expression)) {
      if (!parseExpression())
        return ErrorHandler();
    }
  } else {
    return ErrorHandler();
  }

  return true;
}

bool Parser::parseIfStatement() {
  auto ErrorHandler = [this]() {
    return skipUntil(tok::semicolon, tok::kw_ELSE, tok::kw_END);
  };

  if (!consume(tok::kw_IF))
    return ErrorHandler();

  if (!parseExpression())
    return ErrorHandler();

  if (!consume(tok::kw_THEN))
    return ErrorHandler();

  if (!parseStatementSequence())
    return ErrorHandler();

  if (Tok.is(tok::kw_ELSE)) {
    advance();
    if (!parseStatementSequence())
      return ErrorHandler();
  }
  if (!consume(tok::kw_END))
    return ErrorHandler();

  return true;
}

bool Parser::parseWhileStatement() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!consume(tok::kw_WHILE))
    return ErrorHandler();

  if (!parseExpression())
    return ErrorHandler();

  if (!consume(tok::kw_DO))
    return ErrorHandler();

  if (!parseStatementSequence())
    return ErrorHandler();

  if (!consume(tok::kw_END))
    return ErrorHandler();

  return true;
}
bool Parser::parseExpList() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!parseExpression())
    return ErrorHandler();

  while (Tok.is(tok::comma)) {
    advance();

    if (!parseExpression())
      return ErrorHandler();
  }

  return true;
}

bool Parser::parseExpression() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!parseSimpleExpression())
    return ErrorHandler();

  if (Tok.isOneOf(StartSet.Relation)) {
    if (!parseRelation())
      return ErrorHandler();

    if (!parseSimpleExpression())
      return ErrorHandler();
  }
  return true;
}

bool Parser::parseRelation() {
  auto ErrorHandler = [this]() { return skipUntil(); };
  if (Tok.isOneOf(StartSet.Relation)) {
    advance();
    return true;
  }
  return ErrorHandler();
}

bool Parser::parseSimpleExpression() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (Tok.isOneOf(tok::plus, tok::minus)) {
    advance();
  }

  if (!parseTerm())
    return ErrorHandler();

  while (Tok.isOneOf(StartSet.Term)) {
    if (!parseAddOperator())
      return ErrorHandler();

    if (!parseTerm())
      return ErrorHandler();
  }

  return true;
}

bool Parser::parseAddOperator() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (Tok.isOneOf(StartSet.AddOperator)) {
    advance();
    return true;
  }
  return ErrorHandler();
}

bool Parser::parseTerm() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!parseFactor())
    return ErrorHandler();

  while (Tok.isOneOf(StartSet.MulOperator)) {
    if (!parseMulOperator())
      return ErrorHandler();

    if (!parseFactor())
      return ErrorHandler();
  }

  return true;
}

bool Parser::parseMulOperator() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (Tok.isOneOf(StartSet.MulOperator)) {
    advance();
    return true;
  }
  return ErrorHandler();
}

bool Parser::parseFactor() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  // integer_literal
  if (Tok.is(tok::integer_literal)) {
    advance();
  }
  // "(" expression ")"
  else if (Tok.is(tok::l_paren)) {
    advance();
    if (!parseExpression())
      return ErrorHandler();
    if (!consume(tok::r_paren))
      return ErrorHandler();
  }
  // "NOT" factor
  else if (Tok.is(tok::kw_NOT)) {
    advance();
    if (!parseFactor())
      return ErrorHandler();
  }
  // qualident ( "(" (explist)? ")" )?
  else if (Tok.is(tok::identifier)) {
    if (!parseQualident())
      return ErrorHandler();

    if (Tok.is(tok::l_paren)) {
      advance();

      if (Tok.isOneOf(StartSet.ExpList)) {
        if (!parseExpList())
          return ErrorHandler();
      }

      if (!consume(tok::r_paren))
        return ErrorHandler();
    }
    // TODO: Revisit
  }
  return true;
}

bool Parser::parseQualident() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!consume(tok::identifier))
    return ErrorHandler();

  while (Tok.is(tok::period)) {
    advance();
    if (!consume(tok::identifier))
      return ErrorHandler();
  }

  return true;
}

bool Parser::parseIdentList() {
  auto ErrorHandler = [this]() { return skipUntil(); };

  if (!consume(tok::identifier))
    return ErrorHandler();

  while (Tok.is(tok::comma)) {
    advance();
    if (!consume(tok::identifier))
      return ErrorHandler();
  }

  return true;
}
