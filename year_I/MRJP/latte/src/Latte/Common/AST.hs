{-# LANGUAGE DeriveFunctor #-}

module Latte.Common.AST where

newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data ProgramF a = Program [TopDefF a] a
  deriving (Eq, Ord, Show, Read, Functor)

data TopDefF a = GlobalFunDef (FunDefF a) | ClassDef Ident (Maybe Ident) [FieldDef] [FunDefF a] a
  deriving (Eq, Ord, Show, Read, Functor)

data ClassPropertyF a = MethodDef (FunDefF a) | Field FieldDef
  
data FunDefF a = FunctionDef Type Ident [ArgF a] (BlockF a) a
  deriving (Eq, Ord, Show, Read, Functor)

data ArgF a = Arg Type Ident a
  deriving (Eq, Ord, Show, Read, Functor)

data FieldDef = FieldDef Type Ident
  deriving (Eq, Ord, Show, Read)

data BlockF a = Block [StmtF a] a
  deriving (Eq, Ord, Show, Read, Functor)

data StmtF a
    = Empty a
    | BStmt (BlockF a) a
    | Decl Type [ItemF a] a
    | Ass (ExprF a) (ExprF a) a
    | Incr Ident a
    | Decr Ident a
    | Ret (ExprF a) a
    | VRet a
    | Cond (ExprF a) (StmtF a) a
    | CondElse (ExprF a) (StmtF a) (StmtF a) a
    | While (ExprF a) (StmtF a) a
    | For (ArgF a) (ExprF a) (StmtF a) a
    | SExp (ExprF a) a
  deriving (Eq, Ord, Show, Read, Functor)

data ItemF a = NoInit Ident a | Init Ident (ExprF a) a
  deriving (Eq, Ord, Show, Read, Functor)

data Type = Int | Str | Bool | Void | Fun Type [Type] | Array Type | Class Ident | ClassType Ident [(Ident, Type)] [(Ident, Type)] (Maybe Ident)
  deriving (Eq, Ord, Show, Read)

data ExprF a
    = EVar Ident a
    | ELitInt Integer a
    | ELitTrue a
    | ELitFalse a
    | EApp (ExprF a) [ExprF a] a
    | EString String a
    | Neg (ExprF a) a
    | Not (ExprF a) a
    | EMul (ExprF a) MulOp (ExprF a) a
    | EAdd (ExprF a) AddOp (ExprF a) a
    | ERel (ExprF a) RelOp (ExprF a) a
    | EAnd (ExprF a) (ExprF a) a
    | EOr (ExprF a) (ExprF a) a
    | EArrayNew Type (ExprF a) a
    | EObjNew Ident a
    | EAttrRef (ExprF a) Ident a
    | ESubscript (ExprF a) (ExprF a) a
    | EConcat (ExprF a) (ExprF a) a
    | ENull Type a
    | ESelf a
  deriving (Eq, Ord, Show, Read, Functor)

data AddOp = Plus | Minus
  deriving (Eq, Ord, Show, Read)

data MulOp = Times | Div | Mod
  deriving (Eq, Ord, Show, Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (Eq, Ord, Show, Read)


annotationItem :: ItemF a -> a
annotationItem (NoInit _ a) = a
annotationItem (Init _ _ a) = a

annotationExpr :: ExprF a -> a
annotationExpr (EVar _ a) = a
annotationExpr (ELitInt _ a) = a
annotationExpr (ELitTrue a) = a
annotationExpr (ELitFalse a) = a
annotationExpr (EApp _ _ a) = a
annotationExpr (EString _ a) = a
annotationExpr (Neg _ a) = a
annotationExpr (Not _ a) = a
annotationExpr (EMul _ _ _ a) = a
annotationExpr (EAdd _ _ _ a) = a
annotationExpr (ERel _ _ _ a) = a
annotationExpr (EAnd _ _ a) = a
annotationExpr (EOr _ _ a) = a
annotationExpr (EArrayNew _ _ a) = a
annotationExpr (EAttrRef _ _ a) = a
annotationExpr (ESubscript _ _ a) = a
annotationExpr (EConcat _ _ a) = a
annotationExpr (EObjNew _ a) = a
annotationExpr (ENull _ a) = a
annotationExpr (ESelf a) = a


annotationStmt :: StmtF a -> a
annotationStmt (Empty a) = a
annotationStmt (BStmt _ a) = a
annotationStmt (Decl _ _ a) = a
annotationStmt (Ass _ _ a) = a
annotationStmt (Incr _ a) = a
annotationStmt (Decr _ a) = a
annotationStmt (Ret _ a) = a
annotationStmt (VRet a) = a
annotationStmt (Cond _ _ a) = a
annotationStmt (CondElse _ _ _ a) = a
annotationStmt (While _ _ a) = a
annotationStmt (For _ _ _ a) = a
annotationStmt (SExp _ a) = a



annotationBlock :: BlockF a -> a
annotationBlock (Block _ a) = a
