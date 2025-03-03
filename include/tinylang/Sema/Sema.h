#ifndef TINYLANG_SEMA_SEMA_H
#define TINYLANG_SEMA_SEMA_H

#include "tinylang/AST/AST.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Sema/Scope.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>

using namespace tinylang;

class Sema {

  friend class EnterDeclScope;
  void enterScope(Decl *D);
  void leaveScope();

  bool isOperatorForType(tok::TokenKind Op, DeclType *Ty);

  void checkFormalAndActualParameters(llvm::SMLoc Loc,
                                      const FormParList &Formals,
                                      const ExprList &Actuals);

  Scope *CurrentScope;
  Decl *CurrentDecl;
  DiagnosticsEngine Diags;

  DeclType *IntegerType;
  DeclType *BooleanType;
  BooleanLiteral *TrueLiteral;
  BooleanLiteral *FalseLiteral;
  DeclConst *TrueConst;
  DeclConst *FalseConst;

public:
  Sema(DiagnosticsEngine &Diags)
      : CurrentScope(nullptr), CurrentDecl(nullptr), Diags(Diags) {
    initiazise();
  };
  void initiazise();

  void actOnModuleDeclaration(DeclModule *&D, Ident *Id);
  void actOnModuleDeclaration(DeclModule *&D, Ident *Id, DeclList &Decls,
                              StatSeq &Stats);
  void actOnImport(Ident *Id, IdentList &ImportList);
  void actOnBlock(DeclList &Decls, StatSeq Stats);

  void actOnProcedureDeclaration(DeclProc *&ProcDecl, Ident *Id);
  void actOnProcedureHeading(DeclProc *ProcDecl, FormParList &Params,
                             Decl *RetType);
  void actOnProcedureDeclaration(DeclProc *ProcDecl, Ident *&Id,
                                 FormParList &Params, Decl *RetType,
                                 DeclList &Decls, StatSeq &Stats);
  void actOnVariableDeclaration(DeclList &Decls, IdentList &Ids, Decl *D);
  void actOnConstantDeclaration(DeclList &Decls, Ident *Id, Expr *E);
  void actOnQualidentPart(Decl *&D, Ident *Id);
  void actOnFormalParameterDeclaration(FormParList &Params, IdentList &Ids,
                                       Decl *D, bool IsVar);

  void actOnAssignment(StatSeq Stats, llvm::SMLoc Loc, Decl *D, Expr *E);
  void actOnProcCall(StatSeq Stats, llvm::SMLoc Loc, Decl *D, ExprList &Exprs);
  void actOnWhileStatement(StatSeq Stats, llvm::SMLoc Loc, Expr *Cond,
                           StatSeq LoopStats);
  void actOnIfStatement(StatSeq Stats, llvm::SMLoc Loc, Expr *Cond,
                        StatSeq Then, StatSeq Else);
  void actOnReturnStatement(StatSeq Stats, llvm::SMLoc Loc, Expr *E);
  void actOnExpression(Expr *&Dest, Expr *Left, Expr *Right, OpInfo &Op);
  void actOnSimpleExpression(Expr *&Dest, Expr *Left, Expr *Right, OpInfo &Op);
  void actOnPrefixExpression(Expr *&Dest, Expr *E, OpInfo &PrefixOp);
  void actOnTerm(Expr *&Dest, Expr *Left, Expr *Right, OpInfo &Op);
  void actOnFunctionCall(Expr *&Dest, Decl *D, ExprList &Params);
  void actOnIntegerLiteral(Expr *&Dest, llvm::SMLoc Loc,
                           llvm::StringRef Literal);
  void actOnVariable(Expr *&Dest, Expr *&E, Decl *D);
};

class EnterDeclScope {
  Sema &Semantics;

public:
  EnterDeclScope(Sema &Semantics, Decl *D) : Semantics(Semantics) {
    Semantics.enterScope(D);
  }
  ~EnterDeclScope() { Semantics.leaveScope(); }
};
#endif
