module AnalyzeInstant where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified AbsInstant

import LexInstant
import ParInstant
import SkelInstant
import ErrM

data ValidationError
  = UndefinedVariable (Set AbsInstant.Ident) Integer
  | ParseError String
  deriving (Eq, Ord, Read)

data ValidationState = ValidationState {
  definedVariables :: Set AbsInstant.Ident,
  line :: Integer
}

type Ident = AbsInstant.Ident
type ValidationResult = Either ValidationError ValidationState
newtype StackSize = StackSize {
  stackSize :: Integer
}
newtype LocalsStore = LocalsStore {
  locals :: Set AbsInstant.Ident
}
type ASTNodeStats = (StackSize, LocalsStore)

data ProgramInstant = Prog [StmtInstant] ASTNodeStats

data StmtInstant = SAss AbsInstant.Ident ExpInstant ASTNodeStats | SExp ExpInstant ASTNodeStats

data ExpInstant
    = ExpAdd ExpInstant ExpInstant ASTNodeStats
    | ExpSub ExpInstant ExpInstant ASTNodeStats
    | ExpMul ExpInstant ExpInstant ASTNodeStats
    | ExpDiv ExpInstant ExpInstant ASTNodeStats
    | ExpLit Integer ASTNodeStats
    | ExpVar AbsInstant.Ident ASTNodeStats

localsSize localsStore = Set.size $ locals localsStore

getStatsStmt :: StmtInstant -> ASTNodeStats
getStatsStmt (SAss _ _ s) = s
getStatsStmt (SExp _ s) = s

getStatsExp :: ExpInstant -> ASTNodeStats
getStatsExp (ExpAdd _ _ s) = s
getStatsExp (ExpSub _ _ s) = s
getStatsExp (ExpMul _ _ s) = s
getStatsExp (ExpDiv _ _ s) = s
getStatsExp (ExpLit _ s) = s
getStatsExp (ExpVar _ s) = s

addStats :: ASTNodeStats -> ASTNodeStats -> ASTNodeStats
addStats (StackSize n1, LocalsStore locals1) (StackSize n2, LocalsStore locals2) =
  (StackSize $ if n1 == n2 then n1 + 1 else max n1 n2, LocalsStore $ Set.union locals1 locals2)
  
  
mergeStats :: ASTNodeStats -> ASTNodeStats -> ASTNodeStats
mergeStats (StackSize n1, LocalsStore locals1) (StackSize n2, LocalsStore locals2) =
  (StackSize $ max n1 n2, LocalsStore $ Set.union locals1 locals2)


add1ToStackSize (StackSize n1, LocalsStore locals1) = (StackSize $ n1 + 1, LocalsStore locals1)


compilerFronted :: String -> Either ValidationError ProgramInstant
compilerFronted s = let ts = myLexer s in case pProgram ts of
  Bad s -> Left $ ParseError s
  Ok  tree -> compileIntoEnhancedAST tree


compileIntoEnhancedAST :: AbsInstant.Program -> Either ValidationError ProgramInstant
compileIntoEnhancedAST p =
  case validateProgram p of
    Left err -> Left err
    Right _ -> Right $ anotateProgramWithStats p


anotateProgramWithStats :: AbsInstant.Program -> ProgramInstant
anotateProgramWithStats (AbsInstant.Prog stmts) =
  let stmtInsts = map anotateStmtWithStats stmts in
  Prog stmtInsts $ foldl mergeStats (StackSize 0, LocalsStore Set.empty) $ map getStatsStmt stmtInsts


anotateStmtWithStats :: AbsInstant.Stmt -> StmtInstant
anotateStmtWithStats stmt =
  case stmt of
    AbsInstant.SAss ident exp -> let expIns1 = anotateExpWithStats exp in SAss ident expIns1 (addStats (StackSize 0, LocalsStore $ Set.singleton ident) $ getStatsExp expIns1)
    AbsInstant.SExp exp -> let expIns1 = anotateExpWithStats exp in SExp expIns1 (add1ToStackSize $ getStatsExp expIns1)

anotateExpWithStats :: AbsInstant.Exp -> ExpInstant
anotateExpWithStats exp =
  case exp of
    AbsInstant.ExpAdd exp1 exp2 -> build ExpAdd exp1 exp2
    AbsInstant.ExpSub exp1 exp2 -> build ExpSub exp1 exp2
    AbsInstant.ExpMul exp1 exp2 -> build ExpMul exp1 exp2
    AbsInstant.ExpDiv exp1 exp2 -> build ExpDiv exp1 exp2
    AbsInstant.ExpLit n -> ExpLit n (StackSize 1, LocalsStore Set.empty)
    AbsInstant.ExpVar ident -> ExpVar ident (StackSize 1, LocalsStore $ Set.singleton ident)
    where conv exp1 exp2 = let (expIns1, expIns2) = (anotateExpWithStats exp1, anotateExpWithStats exp2) in (expIns1, expIns2, addStats (getStatsExp expIns1) (getStatsExp expIns2))
          build cons exp1 exp2 = let (expIns1, expIns2, stats) = conv exp1 exp2 in cons expIns1 expIns2 stats

validateProgram :: AbsInstant.Program -> ValidationResult
validateProgram (AbsInstant.Prog stmts) = foldl validateStmt (Right $ ValidationState {definedVariables = Set.empty, line = 1}) stmts


validateStmt :: ValidationResult -> AbsInstant.Stmt -> ValidationResult
validateStmt result@(Left _) _ = result
validateStmt result@(Right (ValidationState definedVariables line)) stmt@(AbsInstant.SExp exp) =
  let neededVars = extractVariables exp in
   if neededVars `Set.isSubsetOf` definedVariables
    then Right $ ValidationState definedVariables $ line + 1
    else Left $ UndefinedVariable (Set.difference neededVars definedVariables) line
validateStmt (Right (ValidationState definedVariables line)) stmt@(AbsInstant.SAss ident exp) =
  let neededVars = extractVariables exp in
    if neededVars `Set.isSubsetOf` definedVariables
     then Right $ ValidationState (Set.union definedVariables $ Set.singleton ident) (line + 1)
     else Left $ UndefinedVariable (Set.difference neededVars definedVariables) line


extractVariables :: AbsInstant.Exp -> Set AbsInstant.Ident
extractVariables = extractVariablesInternal Set.empty


extractVariablesInternal :: Set AbsInstant.Ident -> AbsInstant.Exp -> Set AbsInstant.Ident
extractVariablesInternal s exp =
  case exp of
    AbsInstant.ExpAdd exp1 exp2 -> extractFromExps [exp1, exp2]
    AbsInstant.ExpSub exp1 exp2 -> extractFromExps [exp1, exp2]
    AbsInstant.ExpMul exp1 exp2 -> extractFromExps [exp1, exp2]
    AbsInstant.ExpDiv exp1 exp2 -> extractFromExps [exp1, exp2]
    AbsInstant.ExpLit _ -> s
    AbsInstant.ExpVar ident -> Set.insert ident s
    where extractFromExps = foldl extractVariablesInternal s

instance Show ValidationError where
    show (ParseError s) = s
    show (UndefinedVariable vars line) = "Undefined variables: " ++ show (map (\(AbsInstant.Ident var) -> var) $ Set.toList vars) ++ " on stmt number " ++ show line
