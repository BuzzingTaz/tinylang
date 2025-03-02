#ifndef TINYLANG_AST_AST_H
#define TINYLANG_AST_AST_H

#include "tinylang/Basic/TokenKinds.h"
#include <llvm/ADT/APSInt.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>
#include <vector>

using namespace tinylang;

class Ident;
typedef std::vector<Ident *> IdentList;

class Decl;
typedef std::vector<Decl *> DeclList;

class Import;
typedef std::vector<Import *> Imports;

class Expr;
typedef std::vector<Expr *> ExprList;

class FormPar;
typedef std::vector<FormPar *> FormParList;

class Stat;
typedef std::vector<Stat *> StatSeq;

class Ident {
  llvm::SMLoc Loc;
  llvm::StringRef Name;

public:
  Ident(llvm::SMLoc Loc, llvm::StringRef Name) : Loc(Loc), Name(Name) {}
  llvm::SMLoc getLoc() { return Loc; };
  llvm::StringRef getName() { return Name; };
};

class Decl {

public:
  enum DeclKind { DK_Module, DK_Const, DK_Type, DK_Var, DK_Param, DK_Proc };

private:
  const DeclKind Kind;

protected:
  Decl *EnclosingDecl;
  Ident *Id;

public:
  Decl(DeclKind Kind, Decl *EnclosingDecl, Ident *Id)
      : Kind(Kind), EnclosingDecl(EnclosingDecl), Id(Id) {}

  DeclKind getKind() const { return Kind; }
  Decl *getEnclosingDecl() { return EnclosingDecl; }
  llvm::SMLoc getLocation() { return Id->getLoc(); }
  llvm::StringRef getName() { return Id->getName(); }
  Ident *getId() { return Id; }
};

class DeclModule : public Decl {
  Imports ModuleImports;
  DeclList BlockDeclList;
  StatSeq BlockStatSeq;

public:
  DeclModule(Decl *EnclosingDecl, Ident *Id, DeclList &Decls, StatSeq &Stats)
      : Decl(DK_Module, EnclosingDecl, Id), BlockDeclList(Decls),
        BlockStatSeq(Stats) {};

  const DeclList &getDecls() { return BlockDeclList; }
  void setDecls(DeclList &D) { BlockDeclList = D; }
  const StatSeq &getStmts() { return BlockStatSeq; }
  void setStmts(StatSeq &L) { BlockStatSeq = L; }

  static bool classof(const Decl *D) { return D->getKind() == DK_Module; }
};

class DeclType : public Decl {
public:
  DeclType(Decl *EnclosingDecl, Ident *Id) : Decl(DK_Type, EnclosingDecl, Id) {}

  static bool classof(const Decl *D) { return D->getKind() == DK_Type; }
};

class DeclVar : public Decl {
  DeclType *Ty;

public:
  DeclVar(Decl *EnclosingDecl, Ident *Id, DeclType *Ty)
      : Decl(DK_Var, EnclosingDecl, Id), Ty(Ty) {}

  DeclType *getType() { return Ty; }

  static bool classof(const Decl *D) { return D->getKind() == DK_Var; }
};

class DeclConst : public Decl {
  Expr *E;

public:
  DeclConst(Decl *EnclosingDecl, Ident *Id, Expr *E)
      : Decl(DK_Const, EnclosingDecl, Id), E(E) {}

  Expr *getExpr() { return E; }

  static bool classof(const Decl *D) { return D->getKind() == DK_Const; }
};

class DeclFormPar : public Decl {
  DeclType *Ty;
  bool IsVar;

public:
  DeclFormPar(Decl *EnclosingDecl, Ident *Id, DeclType *Ty, bool IsVar)
      : Decl(DK_Param, EnclosingDecl, Id), Ty(Ty), IsVar(IsVar) {};

  DeclType *getType() { return Ty; }
  bool isVar() { return IsVar; }
  static bool classof(const Decl *D) { return D->getKind() == DK_Param; }
};

class DeclProc : public Decl {
  FormParList FormParams;
  DeclList BlockDeclList;
  StatSeq BlockStatSeq;
  DeclType *RetType;

public:
  DeclProc(Decl *EnclosingDecl, Ident *Id, FormParList Params,
           DeclType *RetType, DeclList &Decls, StatSeq &Stats)
      : Decl(DK_Proc, EnclosingDecl, Id), FormParams(Params),
        BlockDeclList(Decls), BlockStatSeq(Stats), RetType(RetType) {}

  const FormParList &getFormalParams() { return FormParams; }
  void setFormalParams(FormParList &FP) { FormParams = FP; }
  const DeclList &getDecls() { return BlockDeclList; }
  void setDecls(DeclList &D) { BlockDeclList = D; }
  const StatSeq &getStmts() { return BlockStatSeq; }
  void setStmts(StatSeq &L) { BlockStatSeq = L; }
  DeclType *getRetType() { return RetType; }
  void setRetType(DeclType *Ty) { RetType = Ty; };

  static bool classof(const Decl *D) { return D->getKind() == DK_Proc; }
};

class OpInfo {
  llvm::SMLoc Loc;
  uint32_t Kind;
  bool IsUnspecified : 1;

public:
  OpInfo() : Loc(), Kind(tok::unknown), IsUnspecified(true) {};
  OpInfo(llvm::SMLoc Loc, tok::TokenKind Kind)
      : Loc(Loc), Kind(Kind), IsUnspecified(false) {}

  llvm::SMLoc getLocation() const { return Loc; }
  tok::TokenKind getKind() const { return static_cast<tok::TokenKind>(Kind); }
  bool isUnspecified() const { return IsUnspecified; }
};

class Expr {
public:
  enum ExprKind {
    EK_Infix,
    EK_Prefix,
    EK_Int,
    EK_Bool,
    EK_Var,
    EK_Const,
    EK_Func,
  };

private:
  const ExprKind Kind;
  DeclType *Ty;
  bool IsConstant;

protected:
  Expr(ExprKind Kind, DeclType *Ty, bool IsConst)
      : Kind(Kind), Ty(Ty), IsConstant(IsConst) {};

public:
  ExprKind getKind() const { return Kind; }
  DeclType *getType() { return Ty; }
  void setType(DeclType *T) { Ty = T; }
  bool isConst() { return IsConstant; }
};

class InfixExpression : public Expr {
  Expr *Left;
  Expr *Right;
  const OpInfo Op;

public:
  InfixExpression(Expr *Left, Expr *Right, OpInfo Op, DeclType *Ty,
                  bool IsConst)
      : Expr(EK_Infix, Ty, IsConst), Left(Left), Right(Right), Op(Op) {}

  Expr *getLeft() { return Left; }
  Expr *getRight() { return Right; }
  const OpInfo &getOperatorInfo() { return Op; }

  static bool classof(const Expr *E) { return E->getKind() == EK_Infix; }
};

class PrefixExpression : public Expr {
  Expr *E;
  const OpInfo Op;

public:
  PrefixExpression(Expr *E, OpInfo Op, DeclType *Ty, bool IsConst)
      : Expr(EK_Prefix, Ty, IsConst), E(E), Op(Op) {}

  Expr *getExpr() { return E; }
  const OpInfo &getOperatorInfo() { return Op; }

  static bool classof(const Expr *E) { return E->getKind() == EK_Prefix; }
};

class IntegerLiteral : public Expr {
  llvm::SMLoc Loc;
  llvm::APSInt Value;

public:
  IntegerLiteral(llvm::SMLoc Loc, const llvm::APSInt &Value, DeclType *Ty)
      : Expr(EK_Int, Ty, true), Loc(Loc), Value(Value) {}
  llvm::APSInt &getValue() { return Value; }

  static bool classof(const Expr *E) { return E->getKind() == EK_Int; }
};

class BooleanLiteral : public Expr {
  bool Value;

public:
  BooleanLiteral(bool Value, DeclType *Ty)
      : Expr(EK_Bool, Ty, true), Value(Value) {}
  bool getValue() { return Value; }

  static bool classof(const Expr *E) { return E->getKind() == EK_Bool; }
};

class VariableAccess : public Expr {
  Decl *Var;

public:
  VariableAccess(DeclVar *Var)
      : Expr(EK_Var, Var->getType(), false), Var(Var) {}
  VariableAccess(DeclFormPar *Param)
      : Expr(EK_Var, Param->getType(), false), Var(Param) {}

  Decl *getDecl() { return Var; }

  static bool classof(const Expr *E) { return E->getKind() == EK_Var; }
};

class ConstantAccess : public Expr {
  DeclConst *Const;

public:
  ConstantAccess(DeclConst *Const)
      : Expr(EK_Const, Const->getExpr()->getType(), true), Const(Const) {}

  DeclConst *geDecl() { return Const; }

  static bool classof(const Expr *E) { return E->getKind() == EK_Const; }
};

class FunctionCallExpr : public Expr {
  DeclProc *Proc;
  ExprList Params;

public:
  FunctionCallExpr(DeclProc *Proc, ExprList Params)
      : Expr(EK_Func, Proc->getRetType(), false), Proc(Proc), Params(Params) {}

  DeclProc *geDecl() { return Proc; }
  const ExprList &getParams() { return Params; }

  static bool classof(const Expr *E) { return E->getKind() == EK_Func; }
};

class Stat {
public:
  enum StatKind { ST_QAss, ST_FunCall, ST_If, ST_While, ST_Ret };

private:
  const StatKind Kind;

protected:
  Stat(StatKind Kind) : Kind(Kind) {};

public:
  StatKind getKind() const { return Kind; }
};

class StatQAss : public Stat {
  DeclVar *Var;
  Expr *E;

public:
  StatQAss(DeclVar *Var, Expr *E) : Stat(ST_QAss), Var(Var), E(E) {}

  DeclVar *getVar() { return Var; }
  Expr *getExpr() { return E; }

  static bool classof(const Stat *D) { return D->getKind() == ST_QAss; }
};

class StatFunCall : public Stat {
  DeclProc *Proc;
  ExprList Params;

public:
  StatFunCall(DeclProc *Proc, ExprList &Params)
      : Stat(ST_FunCall), Proc(Proc), Params(Params) {}

  DeclProc *getProc() { return Proc; }
  const ExprList &getParams() { return Params; }

  static bool classof(const Stat *D) { return D->getKind() == ST_FunCall; }
};

class StatIf : public Stat {
  Expr *Cond;
  StatSeq Then, Else;

public:
  StatIf(Expr *Cond, StatSeq &Then, StatSeq &Else)
      : Stat(ST_If), Cond(Cond), Then(Then), Else(Else) {}

  Expr *getCond() { return Cond; }
  const StatSeq &getThen() { return Then; }
  const StatSeq &getElse() { return Else; }

  static bool classof(const Stat *D) { return D->getKind() == ST_If; }
};

class StatWhile : public Stat {
  Expr *Cond;
  StatSeq Body;

public:
  StatWhile(Expr *Cond, StatSeq &Body)
      : Stat(ST_While), Cond(Cond), Body(Body) {}

  Expr *getCond() { return Cond; }
  const StatSeq &getBody() { return Body; }

  static bool classof(const Stat *D) { return D->getKind() == ST_While; }
};

class StatRet : public Stat {
  Expr *RetVal;

public:
  StatRet(Expr *RetVal) : Stat(ST_Ret), RetVal(RetVal) {}

  Expr *getRetVal() { return RetVal; }
  static bool classof(const Stat *D) { return D->getKind() == ST_Ret; }
};

#endif
