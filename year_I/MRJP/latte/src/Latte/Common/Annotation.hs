module Latte.Common.Annotation where

import Text.Megaparsec.Pos

import Latte.Common.AST
import Latte.Common.SymbolTable (SymbolName)


data ParseAnnotation = ParseAnnotation {
  start :: SourcePos,
  end :: SourcePos
} deriving (Eq, Show)

mkParseAnnotation :: SourcePos -> SourcePos -> ParseAnnotation
mkParseAnnotation = ParseAnnotation


mergeParseAnnotation :: ParseAnnotation -> ParseAnnotation -> ParseAnnotation
mergeParseAnnotation (ParseAnnotation start_ _) (ParseAnnotation _ end_) = ParseAnnotation start_ end_


type ArgPar = ArgF ParseAnnotation
type ItemPar = ItemF ParseAnnotation
type ExprPar = ExprF ParseAnnotation
type StmtPar = StmtF ParseAnnotation
type BlockPar = BlockF ParseAnnotation
type FunDefPar = FunDefF ParseAnnotation
type ClassDeclPar = ClassPropertyF ParseAnnotation
type TopDefPar = TopDefF ParseAnnotation
type ProgramPar = ProgramF ParseAnnotation

data TypecheckerAnnotation = TypecheckerAnnotation {
  annotationType :: Type,
  doesNotReturn :: Bool,
  newSymbolName :: Maybe SymbolName
} deriving (Eq, Show)

emptyTypecheckerAnnotation, eta :: TypecheckerAnnotation
emptyTypecheckerAnnotation = TypecheckerAnnotation Void True Nothing
eta = emptyTypecheckerAnnotation


putSymbolName :: SymbolName -> TypecheckerAnnotation -> TypecheckerAnnotation
putSymbolName symName (TypecheckerAnnotation t r _) = TypecheckerAnnotation t r $ Just symName


typecheckerToSymbol :: TypecheckerAnnotation -> Maybe SymbolName
typecheckerToSymbol (TypecheckerAnnotation _ _ n) = n

typecheckerToType :: TypecheckerAnnotation -> Type
typecheckerToType (TypecheckerAnnotation t _ _) = t

type ArgType = ArgF TypecheckerAnnotation
type ItemType = ItemF TypecheckerAnnotation
type ExprType = ExprF TypecheckerAnnotation
type StmtType = StmtF TypecheckerAnnotation
type BlockType = BlockF  TypecheckerAnnotation
type FunDefType = FunDefF TypecheckerAnnotation
type TopDefType = TopDefF TypecheckerAnnotation
type ProgramType = ProgramF TypecheckerAnnotation
