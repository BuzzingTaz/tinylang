#include "tinylang/Parser/Parser.h"
#include "tinylang/Basic/TokenKinds.h"

AST *Parser::parse() {
  AST *Res = parseProgram();
  expect(tok::TokenKind::eof);
  return Res;
}

// AST *Parser::parseProgram() {
//   Expr *E;
//   llvm::SmallVector<llvm::StringRef, 8> Vars;

//   if (Tok.is(Token::KW_with)) {
//     advance();

//     if (expect(Token::ident))
//       goto _error;
//     Vars.push_back(Tok.getText());
//     advance();
//     while (Tok.is(Token::comma)) {
//       advance();
//       if (expect(Token::ident))
//         goto _error;
//       Vars.push_back(Tok.getText());
//       advance();
//     }
//     if (consume(Token::colon))
//       goto _error;
//   }
//   E = parseExpr();

//   if (Vars.empty())
//     return E;
//   else
//     return new WithDecl(Vars, E);

// _error:
//   while (!Tok.is(Token::eoi))
//     advance();
//   return nullptr;
// }

// Expr *Parser::parseExpr() {
//   Expr *Left = parseTerm();
//   while (Tok.isOneOf(Token::plus, Token::minus)) {
//     BinaryOp::Operator Op =
//         Tok.is(Token::plus) ? BinaryOp::Plus : BinaryOp::Minus;
//     advance();
//     Expr *Right = parseTerm();
//     Left = new BinaryOp(Op, Left, Right);
//   }
//   return Left;
// }

// Expr *Parser::parseTerm() {
//   Expr *Left = parseFactor();
//   while (Tok.isOneOf(Token::star, Token::slash)) {
//     BinaryOp::Operator Op = Tok.is(Token::star) ? BinaryOp::Mul :
//     BinaryOp::Div; advance(); Expr *Right = parseFactor(); Left = new
//     BinaryOp(Op, Left, Right);
//   }
//   return Left;
// }

// Expr *Parser::parseFactor() {
//   Expr *Res = nullptr;
//   switch (Tok.getKind()) {
//   case Token::number:
//     Res = new Factor(Factor::Number, Tok.getText());
//     advance();
//     break;
//   case Token::ident:
//     Res = new Factor(Factor::Ident, Tok.getText());
//     advance();
//     break;
//   case Token::l_paren:
//     advance();
//     Res = parseExpr();
//     if (!consume(Token::r_paren))
//       break;
//   default:
//     if (!Res)
//       error();
//     while (!Tok.isOneOf(Token::r_paren, Token::star, Token::slash,
//     Token::plus,
//                         Token::minus, Token::eoi))
//       advance();
//   }
//   return Res;
// }
AST *Parser::parseProgram() {}

void Parser::parseImport() {}

void Parser::parseBlock() {}

void Parser::parseDeclaration() {}
void Parser::parseConstantDeclaration() {}
void Parser::parseVariableDeclaration() {}
void Parser::parseProcedureDeclaration() {}
void Parser::parseFormalParameters() {}
void Parser::parseFormalParametersList() {}
void Parser::parseFormalParameter() {}
void Parser::parseIdentList() {}
void Parser::parseQualident() {}

bool Parser::parseStatementSequence() {}
void Parser::parseStatement() {}

bool Parser::parseIfStatement() {
  auto _errorHander = [this]() {
    return skipUntil(tok::semicolon, tok::kw_ELSE, tok::kw_END);
  };

  if (!consume(tok::kw_IF))
    return _errorHander();

  if (!parseExpression())
    return _errorHander();

  if (!consume(tok::kw_THEN))
    return _errorHander();

  if (!parseStatementSequence())
    return _errorHander();

  if (Tok.is(tok::kw_ELSE)) {
    advance();
    if (!parseStatementSequence())
      return _errorHander();
  }
  if (!expect(tok::kw_END))
    return _errorHander();
  return false;
}

void Parser::parseWhileStatement() {}
void Parser::parseExpList() {}
bool Parser::parseExpression() { return true; }
void Parser::parseRelation() {}
void Parser::parseSimpleExpression() {}
void Parser::parseAdd() {}
void Parser::parseTerm() {}
void Parser::parseMulOperator() {}
void Parser::parseFactor() {}