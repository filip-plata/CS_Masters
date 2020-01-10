module CompileJVMInstant where

import Data.Map (Map)
import Control.Monad.State
import Control.Monad
import qualified Data.Map as Map
import AnalyzeInstant
import Control.Monad.Writer.Lazy (WriterT, tell, execWriterT, appEndo, Endo(Endo))


data InstructionJVM
  = Store Integer
  | Load Integer
  | PrintInt
  | LoadOut
  | Push Integer
  | Preambule String StackSize Integer
  | Postambule
  | Add
  | Mul
  | Sub
  | Div
  | Swap

data CompilerJVMState = CompilerJVMState {
  locationMap :: Map Ident Integer, -- mapping of variables to locations
  maxLocal :: Integer -- maximum location used by variable
}

type CompilerJVM = WriterT (Endo [InstructionJVM]) (State CompilerJVMState)


compileIntermediate :: String -> ProgramInstant -> [InstructionJVM]
compileIntermediate baseName p = evalState (execWriterT (compileProgramJVM baseName p) >>= (\v -> return $ appEndo v [])) (CompilerJVMState Map.empty 1)


compileProgramJVM :: String -> ProgramInstant -> CompilerJVM ()
compileProgramJVM baseName (Prog stmts stats) =
  do
  tell $ Endo ([Preambule baseName (fst stats) (toInteger $ localsSize $ snd stats)]<>)
  mapM_ compileStmtJVM stmts
  tell $ Endo ([Postambule]<>)

compileStmtJVM :: StmtInstant -> CompilerJVM ()
compileStmtJVM (SExp exp _) = tell (Endo ([LoadOut]<>)) >> compileExpJVM exp >> tell (Endo ([PrintInt]<>))
compileStmtJVM (SAss ident exp _)
  = do
    compileExpJVM exp
    st <- get
    let (newState, loc) = allocIfNeeded ident st
    put newState
    tell $ Endo ([Store loc]<>)

compileExpJVM :: ExpInstant -> CompilerJVM ()
compileExpJVM (ExpLit n _) = tell $ Endo ([Push n]<>)
compileExpJVM (ExpVar ident _) = get >>= (\st -> tell $ Endo ([Load $ Map.findWithDefault 0 ident $ locationMap st]<>))
compileExpJVM (ExpAdd exp1 exp2 _) = compileExpBinaryJVM Add exp1 exp2
compileExpJVM (ExpSub exp1 exp2 _) = compileExpBinaryJVM Sub exp1 exp2
compileExpJVM (ExpMul exp1 exp2 _) = compileExpBinaryJVM Mul exp1 exp2
compileExpJVM (ExpDiv exp1 exp2 _) = compileExpBinaryJVM Div exp1 exp2

compileExpBinaryJVM :: InstructionJVM -> ExpInstant -> ExpInstant -> CompilerJVM ()
compileExpBinaryJVM binOp exp1 exp2 =
  let (stackSize1, stackSize2) = (stackSize $ fst $ getStatsExp exp1, stackSize $ fst $ getStatsExp exp2) in
  do
  swap <- if stackSize1 >= stackSize2
            then compileExprs2 exp1 exp2 >> return False
            else compileExprs2 exp2 exp1 >> return True
  Control.Monad.when (swap && not (commutative binOp)) $ tell $ Endo ([Swap]<>)
  tell $ Endo ([binOp]<>)

commutative :: InstructionJVM -> Bool
commutative Add = True
commutative Mul = True
commutative Div = False
commutative Sub = False

compileExprs2 :: ExpInstant -> ExpInstant -> CompilerJVM ()
compileExprs2 e1 e2 =
  do
    compileExpJVM e1
    compileExpJVM e2

allocIfNeeded :: Ident -> CompilerJVMState -> (CompilerJVMState, Integer)
allocIfNeeded ident st@CompilerJVMState {locationMap = locationMap, maxLocal = maxLocal }
  = let loc = Map.lookup ident locationMap in
    case loc of
      Just l -> (st, l)
      Nothing -> let newMax = maxLocal + 1 in
        (CompilerJVMState (Map.insert ident maxLocal locationMap) newMax, maxLocal)
