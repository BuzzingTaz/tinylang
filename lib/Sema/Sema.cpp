#include "tinylang/Sema/Sema.h"
#include "tinylang/AST/AST.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Sema/Scope.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/SMLoc.h>

using namespace tinylang;

void Sema::enterScope(Decl *D) {
  CurrentScope = new Scope(CurrentScope);
  CurrentDecl = D;
}

void Sema::leaveScope() {
  Scope *Parent = CurrentScope->getParent();
  delete CurrentScope;
  CurrentScope = Parent;
  CurrentDecl = CurrentDecl->getEnclosingDecl();
}

bool Sema::isOperatorForType(tok::TokenKind Op, DeclType *Ty) {
  switch (Op) {
  case tok::plus:
  case tok::minus:
  case tok::star:
  case tok::kw_DIV:
  case tok::kw_MOD:
    return Ty == IntegerType;
  case tok::slash:
    return false; // REAL not implemented
  case tok::kw_AND:
  case tok::kw_OR:
  case tok::kw_NOT:
    return Ty == BooleanType;
  default:
    llvm_unreachable("Unknown operator");
  }
}

void Sema::checkFormalAndActualParameters(llvm::SMLoc Loc,
                                          const FormParList &Formals,
                                          const ExprList &Actuals) {
  if (Formals.size() != Actuals.size()) {
    Diags.report(Loc, diag::err_wrong_number_of_parameters);
    return;
  }
  auto A = Actuals.begin();
  for (auto I = Formals.begin(), E = Formals.end(); I != E; ++I, ++A) {
    DeclFormPar *F = *I;
    Expr *Arg = *A;
    if (F->getType() != Arg->getType())
      Diags.report(
          Loc, diag::err_type_of_formal_and_actual_parameter_not_compatible);
    if (F->isVar() && llvm::isa<VariableAccess>(Arg))
      Diags.report(Loc, diag::err_var_parameter_requires_var);
  }
}

void Sema::initiazise() {
  CurrentScope = new Scope();
  CurrentDecl = nullptr;
  IntegerType = new DeclType(CurrentDecl, new Ident(llvm::SMLoc(), "INTEGER"));
  BooleanType = new DeclType(CurrentDecl, new Ident(llvm::SMLoc(), "BOOLEAN"));
  TrueLiteral = new BooleanLiteral(true, BooleanType);
  FalseLiteral = new BooleanLiteral(false, BooleanType);
  TrueConst =
      new DeclConst(CurrentDecl, new Ident(llvm::SMLoc(), "TRUE"), TrueLiteral);
  FalseConst = new DeclConst(CurrentDecl, new Ident(llvm::SMLoc(), "FALSE"),
                             FalseLiteral);
  CurrentScope->insert(IntegerType);
  CurrentScope->insert(BooleanType);
  CurrentScope->insert(TrueConst);
  CurrentScope->insert(FalseConst);
}

void Sema::actOnModuleDeclaration(DeclModule *&D, Ident *Id) {
  D = new DeclModule(CurrentDecl, Id);
}

void Sema::actOnModuleDeclaration(DeclModule *&ModDecl, Ident *Id,
                                  DeclList &Decls, StatSeq &Stats) {
  if (Id->getName() != ModDecl->getName()) {
    Diags.report(Id->getLoc(), diag::err_module_identifier_not_equal);
    Diags.report(ModDecl->getLocation(),
                 diag::note_module_identifier_declaration);
  }
  ModDecl->setDecls(Decls);
  ModDecl->setStmts(Stats);
}

void Sema::actOnImport(Ident *Id, IdentList &ImportList) {
  Diags.report(llvm::SMLoc(), diag::err_not_yet_implemented);
  /*CurrentScope->insert();*/
}

void Sema::actOnConstantDeclaration(DeclList &Decls, Ident *Id, Expr *E) {
  assert(CurrentScope && "CurrentScope not set");
  DeclConst *Decl = new DeclConst(CurrentDecl, Id, E);
  if (CurrentScope->insert(Decl))
    Decls.push_back(Decl);
  else
    Diags.report(Id->getLoc(), diag::err_symbold_declared, Id->getName());
}

void Sema::actOnVariableDeclaration(DeclList &Decls, IdentList &Ids, Decl *D) {
  if (DeclType *Ty = llvm::dyn_cast<DeclType>(D)) {
    for (auto *Id : Ids) {
      auto *Decl = new DeclVar(CurrentDecl, Id, Ty);

      if (CurrentScope->insert(Decl))
        Decls.push_back(Decl);
      else
        Diags.report(Id->getLoc(), diag::err_symbold_declared, Id->getName());
    }
  } else if (!Ids.empty()) {
    llvm::SMLoc Loc = Ids.front()->getLoc();
    Diags.report(Loc, diag::err_vardecl_requires_type);
  }
}

void Sema::actOnFormalParameterDeclaration(FormParList &Params, IdentList &Ids,
                                           Decl *D, bool IsVar) {
  assert(CurrentScope && "CurrentScope not set");
  if (DeclType *Ty = llvm::dyn_cast<DeclType>(D)) {
    for (auto &Id : Ids) {

      DeclFormPar *Decl = new DeclFormPar(CurrentDecl, Id, Ty, IsVar);
      if (CurrentScope->insert(Decl))
        Params.push_back(Decl);
      else
        Diags.report(Id->getLoc(), diag::err_symbold_declared, Id->getName());
    }
  } else if (!Ids.empty()) {
    llvm::SMLoc Loc = Ids.front()->getLoc();
    Diags.report(Loc, diag::err_vardecl_requires_type);
  }
}

void Sema::actOnProcedureDeclaration(DeclProc *&ProcDecl, Ident *Id) {
  DeclProc *P = new DeclProc(CurrentDecl, Id);

  if (!CurrentScope->insert(P))
    Diags.report(Id->getLoc(), diag::err_sym_declared, Id->getName());
  ProcDecl = P;
  return;
}

void Sema::actOnProcedureHeading(DeclProc *ProcDecl, FormParList &Params,
                                 Decl *RetType) {
  auto *RetTypeDecl = llvm::dyn_cast<DeclType>(RetType);
  if (!RetTypeDecl and RetType)
    Diags.report(RetType->getLocation(), diag::err_returntype_must_be_type);
  else
    ProcDecl->setRetType(RetTypeDecl);
}
void Sema::actOnProcedureDeclaration(DeclProc *ProcDecl, Ident *&Id,
                                     FormParList &Params, Decl *RetType,
                                     DeclList &Decls, StatSeq &Stats) {
  if (Id->getName() != ProcDecl->getName()) {
    Diags.report(Id->getLoc(), diag::err_proc_identifier_not_equal);
    Diags.report(ProcDecl->getLocation(),
                 diag::note_proc_identifier_declaration);
  }

  ProcDecl->setDecls(Decls);
  ProcDecl->setStats(Stats);

  auto *RetTypeDecl = llvm::dyn_cast_or_null<DeclType>(RetType);
  if (!RetTypeDecl && RetType)
    Diags.report(Id->getLoc(), diag::err_returntype_must_be_type,
                 Id->getName());
  else
    ProcDecl->setRetType(RetTypeDecl);
}

void Sema::actOnAssignment(StatSeq Stats, llvm::SMLoc Loc, Decl *D, Expr *E) {
  if (auto *Var = llvm::dyn_cast<DeclVar>(D)) {
    if (Var->getType() != E->getType()) {
      Diags.report(Loc, diag::err_types_for_operator_not_compatible,
                   tok::getPunctuatorSpelling(tok::defines));
    }
    Stats.push_back(new StatQAss(Var, E));
  } else if (D) {
    Diags.report(Loc, diag::err_unknown);
  }
}

void Sema::actOnProcCall(StatSeq Stats, llvm::SMLoc Loc, Decl *D,
                         ExprList &Params) {
  if (auto *Proc = llvm::dyn_cast<DeclProc>(D)) {
    checkFormalAndActualParameters(Loc, Proc->getFormalParams(), Params);
    if (Proc->getRetType())
      Diags.report(Loc, diag::err_procedure_call_on_nonprocedure);
    Stats.push_back(new StatFunCall(Proc, Params));
  } else if (D) {
    Diags.report(Loc, diag::err_procedure_call_on_nonprocedure);
  }
}
void Sema::actOnWhileStatement(StatSeq Stats, llvm::SMLoc Loc, Expr *Cond,
                               StatSeq LoopStats) {
  if (!Cond)
    Cond = FalseLiteral;

  if (Cond->getType() != BooleanType) {
    Diags.report(Loc, diag::err_while_expr_must_be_bool);
  }
  Stats.push_back(new StatWhile(Cond, LoopStats));
}

void Sema::actOnIfStatement(StatSeq Stats, llvm::SMLoc Loc, Expr *Cond,
                            StatSeq Then, StatSeq Else) {

  if (!Cond)
    Cond = FalseLiteral;

  if (Cond->getType() != BooleanType) {
    Diags.report(Loc, diag::err_while_expr_must_be_bool);
  }
  Stats.push_back(new StatIf(Cond, Then, Else));
}

void Sema::actOnReturnStatement(StatSeq Stats, llvm::SMLoc Loc, Expr *RetVal) {
  auto *Proc = llvm::cast<DeclProc>(CurrentDecl);
  if (Proc->getRetType() and !RetVal)
    Diags.report(Loc, diag::err_function_requires_return);
  else if (!Proc->getRetType() and RetVal)
    Diags.report(Loc, diag::err_procedure_requires_empty_return);
  else if (Proc->getRetType() and RetVal) {
    if (Proc->getRetType() != RetVal->getType())
      Diags.report(Loc, diag::err_function_and_return_type);
  }

  Stats.push_back(new StatRet(RetVal));
}

void Sema::actOnExpression(Expr *&Dest, Expr *Left, Expr *Right, OpInfo &Op) {
  if (!Left) {
    Dest = Right;
    return;
  }
  if (!Right) {
    Dest = Left;
    return;
  }

  if (Left->getType() != Right->getType()) {
    Diags.report(Op.getLocation(), diag::err_types_for_operator_not_compatible),
        tok::getPunctuatorSpelling(Op.getKind());
  }
  bool IsConst = Left->isConst() and Right->isConst();

  Dest = new InfixExpression(Left, Right, Op, BooleanType, IsConst);
}

void Sema::actOnSimpleExpression(Expr *&Dest, Expr *Left, Expr *Right,
                                 OpInfo &Op) {
  if (!Left) {
    Dest = Right;
    return;
  }
  if (!Right) {
    Dest = Left;
    return;
  }

  if (Left->getType() != Right->getType()) {
    Diags.report(Op.getLocation(), diag::err_types_for_operator_not_compatible),
        tok::getPunctuatorSpelling(Op.getKind());
  }

  bool IsConst = Left->isConst() and Right->isConst();
  DeclType *Ty = Left->getType();
  if (IsConst and Op.getKind() == tok::kw_OR) {
    BooleanLiteral *L = llvm::dyn_cast<BooleanLiteral>(Left);
    BooleanLiteral *R = llvm::dyn_cast<BooleanLiteral>(Right);
    Dest = L->getValue() or R->getValue() ? TrueLiteral : FalseLiteral;
    return;
  }
  Dest = new InfixExpression(Left, Right, Op, Ty, IsConst);
  return;
}

void Sema::actOnTerm(Expr *&Dest, Expr *Left, Expr *Right, OpInfo &Op) {
  if (!Left) {
    Dest = Right;
    return;
  }
  if (!Right) {
    Dest = Left;
    return;
  }

  if (Left->getType() != Right->getType()) {
    Diags.report(Op.getLocation(), diag::err_types_for_operator_not_compatible),
        tok::getPunctuatorSpelling(Op.getKind());
  }

  bool IsConst = Left->isConst() and Right->isConst();
  DeclType *Ty = Left->getType();
  if (IsConst and Op.getKind() == tok::kw_AND) {
    BooleanLiteral *L = llvm::dyn_cast<BooleanLiteral>(Left);
    BooleanLiteral *R = llvm::dyn_cast<BooleanLiteral>(Right);
    Dest = L->getValue() and R->getValue() ? TrueLiteral : FalseLiteral;
    return;
  }
  Dest = new InfixExpression(Left, Right, Op, Ty, IsConst);
  return;
}

void Sema::actOnPrefixExpression(Expr *&Dest, Expr *E, OpInfo &PrefixOp) {
  if (!E) {
    Dest = nullptr;
    return;
  }

  if (!isOperatorForType(PrefixOp.getKind(), E->getType())) {
    Diags.report(PrefixOp.getLocation(),
                 diag::err_types_for_operator_not_compatible,
                 tok::getPunctuatorSpelling(PrefixOp.getKind()));
  }

  if (E->isConst() && PrefixOp.getKind() == tok::kw_NOT) {
    BooleanLiteral *L = llvm::dyn_cast<BooleanLiteral>(E);
    Dest = L->getValue() ? FalseLiteral : TrueLiteral;
    return;
  }

  if (PrefixOp.getKind() == tok::minus) {
    bool Ambiguous = true;
    if (llvm::isa<IntegerLiteral>(E) or llvm::isa<VariableAccess>(E) or
        llvm::isa<ConstantAccess>(E))
      Ambiguous = false;
    else if (auto *Infix = llvm::dyn_cast<InfixExpression>(E)) {
      tok::TokenKind Kind = Infix->getOperatorInfo().getKind();
      if (Kind == tok::star || Kind == tok::slash)
        Ambiguous = false;
    }
    if (Ambiguous) {
      Diags.report(PrefixOp.getLocation(), diag::warn_ambigous_negation);
    }
  }

  Dest = new PrefixExpression(E, PrefixOp, E->getType(), E->isConst());
  return;
}

void Sema::actOnIntegerLiteral(Expr *&Dest, llvm::SMLoc Loc,
                               llvm::StringRef Literal) {
  uint8_t Radix = 10;
  if (Literal.endswith("H")) {
    Literal = Literal.drop_back();
    Radix = 16;
  }
  llvm::APInt Value(64, Literal, Radix);
  Dest = new IntegerLiteral(Loc, llvm::APSInt(Value, false), IntegerType);
  return;
}
void Sema::actOnVariable(Expr *&Dest, Expr *&E, Decl *D) {
  if (!D) {
    Dest = nullptr;
    return;
  }
  if (auto *V = llvm::dyn_cast<DeclVar>(D)) {
    Dest = new VariableAccess(V);
    return;
  }
  if (auto *P = llvm::dyn_cast<DeclFormPar>(D)) {
    Dest = new VariableAccess(P);
    return;
  }

  if (auto *C = llvm::dyn_cast<DeclConst>(D)) {
    if (C == TrueConst) {
      Dest = TrueLiteral;
      return;
    }
    if (C == FalseConst) {
      {
        Dest = FalseLiteral;
        return;
      }
    }
    Dest = new ConstantAccess(C);
    return;
  }
  Dest = nullptr;
  return;
}

void Sema::actOnFunctionCall(Expr *&Dest, Decl *D, ExprList &Params) {
  if (!D) {
    Dest = nullptr;
    return;
  }

  if (auto *P = llvm::dyn_cast<DeclProc>(D)) {
    checkFormalAndActualParameters(D->getLocation(), P->getFormalParams(),
                                   Params);
    if (!P->getRetType())
      Diags.report(D->getLocation(), diag::err_function_call_on_nonfunction);
    {
      Dest = new FunctionCallExpr(P, Params);
      return;
    }
  }

  Diags.report(D->getLocation(), diag::err_function_call_on_nonfunction);
  Dest = nullptr;
  return;
}

void Sema::actOnQualidentPart(Decl *&D, Ident *Id) {
  if (!D) {
    if (D = CurrentScope->lookup(Id->getName()))
      return;
  } else if (auto *Mod = llvm::dyn_cast<DeclModule>(D)) {
    auto Decls = Mod->getDecls();
    for (auto I = Decls.begin(), E = Decls.end(); I != E; I++) {
      if ((*I)->getName() == Id->getName()) {
        D = *I;
        return;
      }
    }
  } else {
    llvm_unreachable(
        "actOnQualidentPart only callable with module declarations");
  }

  Diags.report(Id->getLoc(), diag::err_undeclared_name, Id->getName());
  D = nullptr;
}
