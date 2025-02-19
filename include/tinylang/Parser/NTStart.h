#ifndef TINYLANG_PARSER_NTSTART_H
#define TINYLANG_PARSER_NTSTART_H

#include "tinylang/Basic/TokenKinds.h"
#include <unordered_set>

using namespace tinylang;

class NTstart {
public:
  std::unordered_set<tok::TokenKind> IdentList;
  std::unordered_set<tok::TokenKind> Qualident;
  std::unordered_set<tok::TokenKind> Factor;
  std::unordered_set<tok::TokenKind> MulOperator;
  std::unordered_set<tok::TokenKind> Term;
  std::unordered_set<tok::TokenKind> AddOperator;
  std::unordered_set<tok::TokenKind> SimpleExpression;
  std::unordered_set<tok::TokenKind> Relation;
  std::unordered_set<tok::TokenKind> Expression;
  std::unordered_set<tok::TokenKind> ExpList;
  std::unordered_set<tok::TokenKind> WhileStatement;
  std::unordered_set<tok::TokenKind> IfStatement;
  std::unordered_set<tok::TokenKind> Statement;
  std::unordered_set<tok::TokenKind> StatementSequence;
  std::unordered_set<tok::TokenKind> FormalParameter;
  std::unordered_set<tok::TokenKind> FormalParameterList;
  std::unordered_set<tok::TokenKind> FormalParameters;
  std::unordered_set<tok::TokenKind> ProcedureDeclaration;
  std::unordered_set<tok::TokenKind> VariableDeclaration;
  std::unordered_set<tok::TokenKind> ConstantDeclaration;
  std::unordered_set<tok::TokenKind> Declaration;
  std::unordered_set<tok::TokenKind> Block;
  std::unordered_set<tok::TokenKind> Import;
  std::unordered_set<tok::TokenKind> Program;

  NTstart() {
    IdentList = {tok::identifier};

    Qualident = {tok::identifier};

    Factor = {tok::integer_literal, tok::l_paren, tok::kw_NOT, tok::identifier};

    MulOperator = {tok::identifier, tok::slash, tok::kw_DIV, tok::kw_MOD,
                   tok::kw_AND};

    Term = Factor;

    AddOperator = {tok::plus, tok::minus, tok::kw_OR};

    SimpleExpression = Term;
    SimpleExpression = {tok::plus, tok::minus};

    Relation = {tok::equals, tok::hash,  tok::less,
                tok::lesseq, tok::great, tok::greateq};

    Expression = SimpleExpression;

    ExpList = Expression;

    WhileStatement = {tok::kw_WHILE};

    IfStatement = {tok::kw_IF};

    Statement = Qualident;
    Statement.insert(tok::l_paren);
    Statement.insert(IfStatement.begin(), IfStatement.end());
    Statement.insert(WhileStatement.begin(), WhileStatement.end());
    Statement.insert(tok::kw_RETURN);

    StatementSequence = Statement;

    FormalParameter = {tok::kw_VAR};
    FormalParameter.insert(IdentList.begin(), IdentList.end());

    FormalParameterList = FormalParameter;

    FormalParameters = {tok::l_paren};

    ProcedureDeclaration = {tok::kw_PROCEDURE};

    VariableDeclaration = IdentList;

    ConstantDeclaration = {tok::identifier};

    Declaration = {tok::kw_CONST, tok::kw_VAR};
    Declaration.insert(ProcedureDeclaration.begin(),
                       ProcedureDeclaration.end());

    Block = {Declaration.begin(), Declaration.end()};
    Block.insert(tok::kw_BEGIN);

    Import = {tok::kw_FROM, tok::kw_IMPORT};

    Program = {tok::kw_MODULE};
  }
};

#endif