module Latte.Frontend.Typechecker (
  checkType,
  latteIssuePretty,
  LatteIssue
) where

import Latte.Common.Annotation
import Latte.Common.AST
import Latte.Common.Runtime
import qualified Latte.Common.SymbolTable as ST
import Latte.Frontend.Error

import Safe (headMay)

import Control.Monad (unless, when)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List (findIndices, sortBy, find)
import Data.List.Unique (repeated)

import Control.Monad.Identity (runIdentity)

import Control.Exception (assert)
import Latte.Common.SymbolTable (mkSymbolName, getClassMethods, getClassFields, SymbolTableError(..))
import Data.Foldable (foldlM)


data VariableInfo = VariableInfo {
  varType :: Type,
  varSymbolName :: ST.SymbolName,
  isClassVar :: Bool
} deriving (Eq, Show)
type VariableScopes = Map Ident [VariableInfo]
type CurrentScopeVars = Map Ident VariableInfo

data DeclScope = DeclScope {
  scopes :: VariableScopes,
  currentScope :: CurrentScopeVars,
  symbolTable :: ST.SymbolTable
}

emptyDeclScope :: DeclScope
emptyDeclScope = DeclScope Map.empty Map.empty ST.empty


data LatteContext = LatteContext {
  function :: Maybe (Ident, Type),
  classIdent :: Maybe Ident,
  topDefs :: Map Ident Type
}
type TCM a = ExceptT LatteIssue (ReaderT LatteContext (State DeclScope)) a


checkType :: ProgramPar -> (Either LatteIssue ProgramType, ST.SymbolTable)
checkType p = let (r, s) = runIdentity $ runStateT (runReaderT (runExceptT (checkTypeProgram p)) (LatteContext Nothing Nothing Map.empty)) emptyDeclScope in
              (r, symbolTable s)


checkTypeProgram :: ProgramPar -> TCM ProgramType
checkTypeProgram (Program defs _) = do
  topDefs_ <- foldlM buildTopDefTypesMap (Map.fromList runtimeDefinitions) defs
  buildGlobalSymbols $ topologicalSortTopDefs $ Map.toList topDefs_
  checkHasMain topDefs_
  annotatedDefs <- local (\ctx -> ctx {topDefs = topDefs_}) $ mapM checkTypeTopDef defs
  return $ Program annotatedDefs emptyTypecheckerAnnotation


buildGlobalSymbols :: [(Ident, Type)] -> TCM ()
buildGlobalSymbols = mapM_ (uncurry insertGlobalSymbol)


insertGlobalSymbol :: Ident -> Type -> TCM ()
insertGlobalSymbol (Ident s) t = do
  symT <- gets symbolTable
  st <- get
  case ST.insertSymbol (ST.mkSymbolName s) t symT of
    Right newSymTable -> put $ st { symbolTable = newSymTable }
    Left (ClassParentNotDefined _ parent) ->
      -- this must not happen, the caller has to sort definitions properly
      throwError $ LatteError $ GeneralError $ ParentClassUndefined $ Ident parent


buildTopDefTypesMap :: Map Ident Type -> TopDefPar -> TCM (Map Ident Type)
buildTopDefTypesMap topLevelDefs def =
  case def of
    GlobalFunDef f@(FunctionDef _ ident _ _ info) ->
      if Map.notMember ident topLevelDefs
        then return $ Map.insert ident (snd . funDefToType $ f) topLevelDefs
        else throwPositionalError (RedeclarationFunction ident, info)
    ClassDef name parentNameM fields methods info -> do
      when (Map.member name topLevelDefs) $ throwPositionalError (RedeclarationClass name, info)
      checkForFieldRedeclaration name fields info
      return $ Map.insert name
                (ClassType name (map (\(FieldDef t ident) -> (ident, t)) fields) (map funDefToType methods) parentNameM) topLevelDefs
  where
      funDefToType (FunctionDef rType ident args _ _) = (ident, Fun rType $ argTypes args)
      argType (Arg t _ _) = t
      argTypes = map argType


checkTypeTopDef :: TopDefPar -> TCM TopDefType

checkTypeTopDef (GlobalFunDef fDef) = validateFunction fDef >>= \rDef -> return $ GlobalFunDef rDef

checkTypeTopDef (ClassDef classIdent_ mParent fields methods info) = do
  _ <- addVarsToCurrentScope info $ map (\(FieldDef t i) -> (i, t, True)) fields
  valMethods <- local (\ctx -> ctx { classIdent = Just classIdent_ }) $
                  mapM (fmap (addThisToMethodArgs classIdent_) . withNewScope . validateFunction) methods
  clearCurrentScope
  return $ ClassDef classIdent_ mParent fields valMethods emptyTypecheckerAnnotation


validateFunction :: FunDefPar -> TCM FunDefType
validateFunction (FunctionDef rType ident args block info) = do
  let vars = map (\(Arg t i _) -> (i, t, False)) args
  startScopes <- gets currentScope
  assert (Map.null startScopes) $ return ()

  argSymbols <- addVarsToCurrentScope info vars
  let annotatedArgs = map (\(Arg t i _, sym) -> Arg t i $ TypecheckerAnnotation t True (Just sym)) (zip args argSymbols)
  annotatedBlock <- addDummyRet rType <$> local (\ctx -> ctx { function = Just (ident, rType) } ) (checkTypeBlock block)
  clearCurrentScope
  when
   (rType /= Void && doesNotReturn (annotationBlock annotatedBlock))
   (throwPositionalError (NonVoidFunctionMayNotReturn ident, info))
  return $ FunctionDef rType ident annotatedArgs annotatedBlock emptyTypecheckerAnnotation


addThisToMethodArgs :: Ident -> FunDefType -> FunDefType
addThisToMethodArgs className (FunctionDef rType ident args block info) =
  let classType = Class className in
  FunctionDef rType ident (
    Arg classType (Ident selfRefName) (TypecheckerAnnotation classType True $ Just $ mkSymbolName selfRefName)
      : args) block info


addDummyRet :: Type -> BlockType -> BlockType
addDummyRet t b@(Block stmts a) =
  let retA = TypecheckerAnnotation Void False Nothing in
  if not (all (doesNotReturn . annotationStmt) stmts) then b else
  case t of
    Void -> Block (stmts ++ [VRet retA]) (a { doesNotReturn = False })
    _ -> b


checkTypeBlock :: BlockPar -> TCM BlockType
checkTypeBlock b@(Block stmtsAll _) = do
  let stmts = filter (not . isEmptyStmt) stmtsAll
  checkReturnCorrectnes b
  annotatedStmts <- mapM checkTypeStmt stmts
  let filterAfterRet = takeWhileInclusive (not . isRetStmt) annotatedStmts
  return $ Block filterAfterRet $ TypecheckerAnnotation Void (all (doesNotReturn . annotationStmt) filterAfterRet) Nothing


checkTypeStmt :: StmtPar -> TCM StmtType

checkTypeStmt (Empty _) = return $ Empty $ TypecheckerAnnotation Void True Nothing

checkTypeStmt (BStmt block _) =
  withNewBlock block >>= \annotatedBlock -> return . BStmt annotatedBlock $ annotationBlock annotatedBlock


checkTypeStmt (Decl t items _) = do
  annotatedItems <- mapM (checkTypeItem t) items
  return $ Decl t annotatedItems $ TypecheckerAnnotation t True Nothing

checkTypeStmt (Ass lval rval a) = do
  annotatedRval <- checkTypeExpr rval
  annotatedLval <- checkTypeExpr lval
  let rvalType = annotationType $ annotationExpr annotatedRval
  let lvalType = annotationType $ annotationExpr annotatedLval
  isSubclass <- rvalType `isSubclassOf` lvalType

  when (rvalType /= lvalType && not isSubclass) $ throwError $ errWrapPos (AssignmentTypeMismatch lvalType rvalType, a)
  let retVal = Ass annotatedLval annotatedRval $ TypecheckerAnnotation Void True Nothing

  case annotatedLval of
    EVar {} -> return retVal
    ESubscript {} -> return retVal
    EAttrRef {} -> return retVal
    _ -> throwError $ errWrapPos (NotAnLvalue, a)

checkTypeStmt (Incr ident a) = do
  validateVarType ident Int a

  varRef <- checkTypeExpr $ EVar ident a

  return $ Ass varRef (EAdd varRef Plus (ELitInt 1 eta) eta) eta

checkTypeStmt (Decr ident a) = do
  validateVarType ident Int a

  let equivAssignment = Ass (EVar ident a) (EAdd (EVar ident a) Minus (ELitInt 1 a) a) a

  checkTypeStmt equivAssignment

checkTypeStmt (Ret expr a) = do
  annotatedExpr <- checkTypeExpr expr
  let expectedT = annotationType $ annotationExpr annotatedExpr
  when (expectedT == Void) $ throwPositionalError (RetVoidExpr, a)
  checkReturnTypeStmt expectedT a
  return $ Ret annotatedExpr $ TypecheckerAnnotation Void False Nothing

checkTypeStmt (VRet a) = checkReturnTypeStmt Void a >> return (VRet $ TypecheckerAnnotation Void False Nothing)

checkTypeStmt (Cond expr stmt _) = do
  annotatedExpr <- checkTypeExpr expr
  case annotatedExpr of
    ELitTrue _ -> checkTypeStmt stmt
    ELitFalse _ -> return $ Empty $ TypecheckerAnnotation Void True Nothing
    _ -> do
          when (Bool /= annotationType (annotationExpr annotatedExpr)) $ throwError $ errWrapPos (ExpectedBooleanConditional, annotationExpr expr)
          annotatedStmt <- checkTypeStmt stmt
          return $ CondElse annotatedExpr annotatedStmt (BStmt (Block [] emptyTypecheckerAnnotation) emptyTypecheckerAnnotation)
                 $ TypecheckerAnnotation Void True Nothing

checkTypeStmt (CondElse expr stmtTrue stmtFalse _) = do
  annotatedExpr <- checkTypeExpr expr
  when (Bool /= annotationType (annotationExpr annotatedExpr)) $ throwError $ errWrapPos (ExpectedBooleanConditional, annotationExpr expr)
  case annotatedExpr of
    ELitTrue _ -> checkTypeStmt stmtTrue
    ELitFalse _ -> checkTypeStmt stmtFalse
    _ -> do
          annotatedStmtTrue <- checkTypeStmt stmtTrue
          annotatedStmtFalse <- checkTypeStmt stmtFalse
          let stmtTrueDoesNotRet = doesNotReturn $ annotationStmt annotatedStmtTrue
          let stmtFalseDoesNotRet = doesNotReturn $ annotationStmt annotatedStmtFalse
          return $ CondElse annotatedExpr annotatedStmtTrue annotatedStmtFalse $ TypecheckerAnnotation Void (stmtTrueDoesNotRet || stmtFalseDoesNotRet) Nothing

checkTypeStmt (While expr stmt _) = do
  annotatedExpr <- checkTypeExpr expr
  when (Bool /= annotationType (annotationExpr annotatedExpr)) $ throwError $ errWrapPos (ExpectedBooleanConditional, annotationExpr expr)
  case annotatedExpr of
    ELitFalse _ -> return $ Empty $ TypecheckerAnnotation Void True Nothing
    _ -> do
          annotatedStmt <- checkTypeStmt stmt
          return $ While annotatedExpr annotatedStmt $ TypecheckerAnnotation Void (doesNotReturn $ annotationStmt annotatedStmt) Nothing


checkTypeStmt (For (Arg t i _) expr stmt a) = do
  arrayExpr <- checkTypeExpr expr
  let exprType = annotationType (annotationExpr arrayExpr)
  let errorForeach = throwPositionalError (InvalidForeach t, annotationExpr expr)

  case exprType of
    Array insideExprType -> isSubclassOf insideExprType t >>=
      (\isSubclass -> unless (insideExprType == t || isSubclass) errorForeach)
    _ -> errorForeach

  (argSymbols, annotatedStmt) <-
    withNewScope $ addVarsToCurrentScope a [(i, t, False)] >>= (\syms -> checkTypeStmt stmt >>= \an -> return (syms, an))
  iteratorSym <- getFreshVariableSymbol Int
  lengthSym <- getFreshVariableSymbol Int

  -- variable names are disregarded by further compiler phases,
  -- it operates on symbols
  let exprIter = EVar (Ident "__dummy") (putSymbolName iteratorSym eta)
  let exprArrayLength = EVar (Ident "__dummy") (putSymbolName lengthSym eta)
  let exprIterVar = EVar (Ident "__dummy") (putSymbolName (head argSymbols) eta)

  return $
    BStmt
      (Block
         [ Ass exprIter (ELitInt 0 eta) eta
         , Ass exprArrayLength (EAttrRef arrayExpr (Ident "length") eta) eta
         , While
              (ERel exprIter LTH exprArrayLength eta)
              (BStmt
                 (Block
                    [ Ass exprIterVar (ESubscript arrayExpr exprIter eta) eta
                    , annotatedStmt
                    , Ass exprIter (EAdd exprIter Plus (ELitInt 1 eta) eta) eta
                    ]
                    eta)
                 eta)
              eta
         ]
         eta)
      $ annotationStmt annotatedStmt


checkTypeStmt (SExp expr _) =
  checkTypeExpr expr >>= (\annotatedExpr -> return $ SExp annotatedExpr $ TypecheckerAnnotation Void True Nothing)


checkReturnTypeStmt :: Type -> TypeErrorInfo -> TCM ()
checkReturnTypeStmt expectedType info = do
  funInfoMay <- asks function
  (_, retType) <-
    case funInfoMay of
      Just ident -> return ident
      _ -> throwError $ errWrapPos (RetOutsideOfFunction, info)
  isSubclass <- isSubclassOf expectedType retType
  unless (retType == expectedType || isSubclass) $ throwError $ errWrapPos (RetInvalidType retType expectedType, info)


validateVarType :: Ident -> Type -> TypeErrorInfo -> TCM ()
validateVarType ident expectedType info = do
  varTypeM <- getVarType ident
  case varTypeM of
    Just t -> unless (t == expectedType) (throwError $ errWrapPos (UnexpectedIdentifierType expectedType t ident, info))
    Nothing -> throwError $ errWrapPos (UndefinedIdentifier ident, info)


withNewBlock :: BlockPar -> TCM BlockType
withNewBlock block = withNewScope (checkTypeBlock block)

withNewScope :: TCM a -> TCM a
withNewScope computation = do
  st <- get
  entryVars <- gets currentScope
  pushVarsToStacks entryVars
  result <- computation
  leaveSymT <- gets symbolTable
  put $ st { symbolTable = leaveSymT }
  return result

pushVarsToStacks :: CurrentScopeVars -> TCM ()
pushVarsToStacks currentVars = do
  scopes_ <- gets scopes
  st <- get
  put $ st {scopes = Map.foldlWithKey insertVar scopes_ currentVars, currentScope = Map.empty}

insertVar :: VariableScopes -> Ident -> VariableInfo -> VariableScopes
insertVar scopes_ var t =
  let prevStack =  Map.findWithDefault [] var scopes_  in
  Map.insert var (t:prevStack) scopes_


-- | Performs correctness checking and annotating
-- of declaration items
checkTypeItem :: Type -> ItemPar -> TCM ItemType
checkTypeItem t (NoInit i a) = do
  syms <- addVarsToCurrentScope a [(i, t, False)]
  return $ NoInit i $ TypecheckerAnnotation t True $ Just $ head syms
checkTypeItem t (Init i exp_ a) = do
  annotatedExpr <- checkTypeExpr exp_
  let exprType = annotationType (annotationExpr annotatedExpr)
  isSubclass <- exprType `isSubclassOf` t
  if t == exprType || isSubclass
    then addVarsToCurrentScope a [(i, t, False)] >>= \syms -> return (Init i annotatedExpr $ TypecheckerAnnotation t True $ Just $ head syms)
    else throwError $ errWrapPos (AssignmentInvalid i exprType t, a)


-- | Inserts a list of variable declarations to current scope.
-- Checks for redeclaration issues, and if such are detected,
-- reports an error
addVarsToCurrentScope :: TypeErrorInfo -> [(Ident, Type, Bool)] -> TCM [ST.SymbolName]
addVarsToCurrentScope errorInfo vals = do
  -- check for Void type here
  case find (\(_, t, _) -> t == Void) vals of
    Just (i, _, _) -> throwPositionalError (VoidTypeDeclaration i, errorInfo)
    Nothing -> return ()

  currentScope_ <- gets currentScope
  newDefs <- mapM buildDef vals
  let newDefsMap = Map.fromList newDefs
  let redeclaredVars = Map.intersection currentScope_ newDefsMap
  unless (null redeclaredVars) $ throwError $ errWrapPos (VariableRedeclaration $ Map.keys redeclaredVars, errorInfo)
  let redeclarationsInVals = map (\(t, _, _) -> t) $ vals List.\\ List.nub vals
  unless (List.null redeclarationsInVals) $
    throwError $ errWrapPos (VariableRedeclaration redeclarationsInVals, errorInfo)
  let newCurrentScope = Map.union currentScope_ newDefsMap
  st <- get
  put $ st {currentScope = newCurrentScope}
  return $ map (varSymbolName . snd) newDefs


buildDef :: (Ident, Type, Bool) -> TCM (Ident, VariableInfo)
buildDef (i, t, isClassDef) = do
  symName <- getFreshVariableSymbol t
  return (i, VariableInfo t symName isClassDef)


getFreshVariableSymbol :: Type -> TCM ST.SymbolName
getFreshVariableSymbol t = do
  symT <- gets symbolTable
  let (updatedTable, symName) = ST.freshSymbolSuggestion "var" t ST.None symT

  st <- get
  put $ st { symbolTable = updatedTable }
  return symName


checkTypeExpr :: ExprPar -> TCM ExprType
checkTypeExpr (EVar i a) = do
  mClassIdent <- asks classIdent
  varInfoM <- getVarInfo i
  case varInfoM of
    Just (VariableInfo varType_ symName False) -> return $ EVar i $ TypecheckerAnnotation varType_ True $ Just symName
    Just (VariableInfo varType_ _ True) ->
      case mClassIdent of
        Just classIdent_ -> return $ EAttrRef
          (EVar (Ident selfRefName) $ TypecheckerAnnotation (Class classIdent_) True $ Just (mkSymbolName selfRefName))
          i $ TypecheckerAnnotation varType_ True Nothing
        Nothing -> error "Referencing class field outside of a class context ?!"
    Nothing -> throwError $ errWrapPos (UndefinedIdentifier i, a)

checkTypeExpr (ELitInt n a) = checkIntegerLiteralInRange a n  >>
  return (ELitInt n $ TypecheckerAnnotation Int True Nothing)

checkTypeExpr (ELitTrue _) = return $ ELitTrue $ TypecheckerAnnotation Bool True Nothing

checkTypeExpr (ELitFalse _) = return $ ELitFalse $ TypecheckerAnnotation Bool True Nothing

checkTypeExpr (EApp expr exprs a) = do
  annotatedExpr <- checkTypeExpr expr
  let fType = annotationType . annotationExpr $ annotatedExpr
  case fType of
    Fun rType argTypes ->
      if length exprs /= length argTypes
        then throwError $ errWrapPos (InvalidFunctionApplicationArgumentCount (length argTypes) (length exprs), a)
        else do
          annotatedExprs <- mapM validateFunArgExprType $ zip exprs argTypes
          return (EApp annotatedExpr annotatedExprs $ TypecheckerAnnotation rType True Nothing)
    _ -> throwError $ errWrapPos (CallingUnfunctionObject, a)

checkTypeExpr (EString s _) = do
  symTable <- gets symbolTable
  let (updatedTable, symName) = ST.freshSymbolSuggestion "string" Str (ST.S s) symTable
  st <- get
  put $ st { symbolTable = updatedTable }
  return $ EString s $ TypecheckerAnnotation Str True $ Just symName

checkTypeExpr (Neg expr a) = do
  annotatedExpr <- checkTypeExpr expr
  let exprType = (annotationType . annotationExpr) annotatedExpr
  case annotatedExpr of
    ELitInt n a_ -> checkIntegerLiteralInRange a (-n) >> return (ELitInt (-n) a_)
    _ ->
      if exprType == Int
        then return $ Neg annotatedExpr $ TypecheckerAnnotation Int True Nothing
        else throwError $ errWrapPos (ExpressionNegationInvalid exprType, a)

checkTypeExpr (Not expr a) = do
  annotatedExpr <- checkTypeExpr expr
  case annotatedExpr of
    ELitTrue _ -> return $ ELitFalse $ TypecheckerAnnotation Bool True Nothing
    ELitFalse _ -> return $ ELitTrue $ TypecheckerAnnotation Bool True Nothing
    _ -> do
          let exprType = (annotationType . annotationExpr) annotatedExpr
          if exprType == Bool
            then return $ Not annotatedExpr $ TypecheckerAnnotation Bool True Nothing
            else throwError $ errWrapPos (ExpressionInvalidN exprType, a)

checkTypeExpr (EMul expr1 op expr2 a) = checkTypeBinaryExpr (buildExprMul a op) expr1 expr2 [Int] Int a

checkTypeExpr (EAdd expr1 op expr2 a) = do
  (annotatedExpr1, annotatedExpr2) <- checkTypeExpr2 expr1 expr2
  let exprType1 = (annotationType . annotationExpr) annotatedExpr1
  let exprType2 = (annotationType . annotationExpr) annotatedExpr2
  case (exprType1, exprType2, op) of
    (Int, Int, _) -> buildExpr annotatedExpr1 annotatedExpr2 $ TypecheckerAnnotation Int True Nothing
    (Str, Str, Plus) -> buildExpr annotatedExpr1 annotatedExpr2 $ TypecheckerAnnotation Str True Nothing
    _ -> throwError $ errWrapPos (ExpressionAddInvalid exprType1 exprType2, a)
  where
    buildExpr = buildExprAdd op

checkTypeExpr (ERel expr1 EQU expr2 a) = checkTypeEquality expr1 expr2 EQU a

checkTypeExpr (ERel expr1 NE expr2 a) = checkTypeEquality expr1 expr2 NE a

checkTypeExpr (ERel expr1 op expr2 a) = checkTypeBinaryExpr (buildExprRel op) expr1 expr2 [Int] Bool a

checkTypeExpr (EAnd expr1 expr2 a) = checkTypeBinaryExpr buildExprAnd expr1 expr2 [Bool] Bool a

checkTypeExpr (EOr expr1 expr2 a) = checkTypeBinaryExpr buildExprOr expr1 expr2 [Bool] Bool a

checkTypeExpr (EArrayNew t expr a) = do
  lengthInit <- checkTypeExpr expr
  when (Int /= annotationType (annotationExpr lengthInit)) $ throwError $ errWrapPos (InvalidArrayInitialization, a)
  return $ EArrayNew t lengthInit $ TypecheckerAnnotation (Array t) True Nothing

checkTypeExpr (EObjNew ident _) = return $ EObjNew ident $ TypecheckerAnnotation (Class ident) True Nothing

checkTypeExpr (EAttrRef expr attr a) = do
  annotatedExpr <- checkTypeExpr expr
  let exprType = (annotationType . annotationExpr) annotatedExpr
  case exprType of
    Array _ -> case attr of
                 Ident "length" -> return $ EAttrRef annotatedExpr attr $ TypecheckerAnnotation Int True Nothing
                 _ -> throwPositionalError (InvalidAttribute exprType attr, a)
    Class classIdent_@(Ident ident) -> do
      symTable <- gets symbolTable
      let classSym = mkSymbolName ident
      case (getClassFields symTable classSym, getClassMethods symTable classSym) of
        (Just fields, Just methods) -> do
          let fieldTypesMapping = Map.union methods $ Map.fromList fields

          case Map.lookup attr fieldTypesMapping of
            Just fieldType -> return $ EAttrRef annotatedExpr attr $ TypecheckerAnnotation fieldType True Nothing
            Nothing -> throwPositionalError (InvalidAttribute exprType attr, a)

        _ -> throwPositionalError (UndefinedClass classIdent_, a)
    _ -> throwPositionalError (InvalidAttribute exprType attr, a)

checkTypeExpr (ESubscript expr idx a) = do
  annotatedIdx <- checkTypeExpr idx

  when (Int /= annotationType (annotationExpr annotatedIdx)) $ throwError $ errWrapPos (ArraySubscriptNotAnInt, a)

  annotatedExpr <- checkTypeExpr expr
  let exprType = annotationType (annotationExpr annotatedExpr)
  case exprType of
    Array t -> return $ ESubscript annotatedExpr annotatedIdx $ TypecheckerAnnotation t True Nothing
    _ -> throwError $ errWrapPos (SubscriptExpectedArray exprType, a)

checkTypeExpr (ENull t _) = return $ ENull t $ TypecheckerAnnotation t True Nothing

checkTypeExpr (ESelf a) = do
  mClassIdent <- asks classIdent
  case mClassIdent of
    Just ident -> return $ ESelf $ TypecheckerAnnotation (Class ident) True $ Just $ mkSymbolName selfRefName
    Nothing -> throwPositionalError (SelfOutsideMethod, a)

checkTypeExpr _ = error "There must not be other espressions after parser"


checkTypeEquality :: ExprPar -> ExprPar -> RelOp -> ParseAnnotation -> TCM ExprType
checkTypeEquality expr1 expr2 rel a = let constructor = buildExprRel rel in do
  (annotatedExpr1, annotatedExpr2) <- checkTypeExpr2 expr1 expr2
  let exprType1 = (annotationType . annotationExpr) annotatedExpr1
  let exprType2 = (annotationType . annotationExpr) annotatedExpr2
  let result = constructor annotatedExpr1 annotatedExpr2 $ TypecheckerAnnotation Bool True Nothing
  let err = throwError $ errWrapPos (ExpressionEqualityInvalid exprType1 exprType2, a)

  case (exprType1, exprType2) of
    (Bool, Bool) -> result
    (Int, Int) -> result
    (Str, Str) -> result
    (Array t1, Array t2) -> if t1 == t2 then result else err
    (Class _, Class _) -> do
      isSub1 <- isSubclassOf exprType1 exprType2
      isSub2 <- isSubclassOf exprType2 exprType1
      if isSub1 || isSub2 then result else err
    _ -> err


checkTypeExpr2 :: ExprPar -> ExprPar -> ExceptT LatteIssue (ReaderT LatteContext (State DeclScope)) (ExprType, ExprType)
checkTypeExpr2 e1 e2 = do
  aE1 <- checkTypeExpr e1
  aE2 <- checkTypeExpr e2
  return (aE1, aE2)


checkIntegerLiteralInRange :: TypeErrorInfo -> Integer -> TCM ()
checkIntegerLiteralInRange a integer =
  let integerMin = -(2 ^ (31 :: Integer)); integerMax = 2 ^ (31 :: Integer) - 1 in
  when (integer < integerMin || integer > integerMax) $ throwPositionalError (IntegerLiteralOutsideOfRange integer, a)


buildExprMul :: TypeErrorInfo -> MulOp -> ExprType -> ExprType -> TypecheckerAnnotation -> TCM ExprType
buildExprMul errorInfo op expr1 expr2 a =
  case (expr1, expr2, op) of
    (ELitInt n1 _, ELitInt n2 _, Times) -> return $ ELitInt (n1 * n2) a
    (ELitInt n1 _, ELitInt n2 _, Mod) -> return $ ELitInt (n1 `rem` n2) a
    (ELitInt _ _, ELitInt 0 _, Div) -> throwError $ errWrapPos (DivisionByZero, errorInfo)
    (ELitInt n1 _, ELitInt n2 _, Div) -> return $ ELitInt (n1 `quot` n2) a
    _ -> return $ EMul expr1 op expr2 a


buildExprAdd :: AddOp -> ExprType -> ExprType -> TypecheckerAnnotation -> TCM ExprType
buildExprAdd op expr1 expr2 a = return $
  case (expr1, expr2, op) of
    (ELitInt n1 _, ELitInt n2 _, Plus) -> ELitInt (n1 + n2) a
    (ELitInt n1 _, ELitInt n2 _, Minus) -> ELitInt (n1 - n2) a
    _ -> if annotationType (annotationExpr expr1) == Str
           then EConcat expr1 expr2 a
           else EAdd expr1 op expr2 a


buildExprRel :: RelOp -> ExprType -> ExprType -> TypecheckerAnnotation -> TCM ExprType
buildExprRel op expr1 expr2 a = return $
  case (expr1, expr2, op) of
    (ELitTrue _, ELitTrue _, EQU) -> boolToLit True a
    (ELitTrue _, ELitFalse _, EQU) -> boolToLit False a
    (ELitFalse _, ELitTrue _, EQU) -> boolToLit False a
    (ELitFalse _, ELitFalse _, EQU) -> boolToLit True a
    (ELitInt n1 _, ELitInt n2 _, EQU) -> boolToLit (n1 == n2) a
    (ELitInt n1 _, ELitInt n2 _, LTH) -> boolToLit (n1 < n2) a
    (ELitInt n1 _, ELitInt n2 _, LE) -> boolToLit (n1 <= n2) a
    (ELitInt n1 _, ELitInt n2 _, GTH) -> boolToLit (n1 > n2) a
    (ELitInt n1 _, ELitInt n2 _, GE) -> boolToLit (n1 >= n2) a
    (ELitInt n1 _, ELitInt n2 _, NE) -> boolToLit (n1 /= n2) a
    _ -> ERel expr1 op expr2 a


buildExprAnd :: ExprType -> ExprType -> TypecheckerAnnotation -> TCM ExprType
buildExprAnd expr1 expr2 a = return $
  case (expr1, expr2) of
   (ELitFalse _, _) -> ELitFalse a
   (ELitTrue _, ELitTrue _) -> ELitTrue a
   _ -> EAnd expr1 expr2 a


buildExprOr :: ExprType -> ExprType -> TypecheckerAnnotation -> TCM ExprType
buildExprOr expr1 expr2 a = return $
  case (expr1, expr2) of
    (ELitTrue _, _) -> ELitTrue a
    (ELitFalse _, ELitFalse _) -> ELitFalse a
    _ -> EOr expr1 expr2 a


checkTypeBinaryExpr :: (ExprType -> ExprType -> TypecheckerAnnotation -> TCM ExprType) -> ExprPar -> ExprPar -> [Type] -> Type -> TypeErrorInfo -> TCM ExprType
checkTypeBinaryExpr constructor expr1 expr2 expectedTypes exprType info = do
  (annotatedExpr1, annotatedExpr2) <- checkTypeExpr2 expr1 expr2
  let exprType1 = (annotationType . annotationExpr) annotatedExpr1
  let exprType2 = (annotationType . annotationExpr) annotatedExpr2
  if exprType1 == exprType2 && elem exprType1 expectedTypes
    then constructor annotatedExpr1 annotatedExpr2 $ TypecheckerAnnotation exprType True Nothing
    else throwError $ errWrapPos (ExpressionBinaryInvalid exprType1 exprType2 expectedTypes, info)


validateFunArgExprType :: (ExprPar, Type) -> TCM ExprType
validateFunArgExprType (expr, expectedType) = do
  annotatedExpr <- checkTypeExpr expr
  let exprType = annotationType $ annotationExpr annotatedExpr
  isSubclass <- isSubclassOf exprType expectedType
  unless (expectedType == exprType || isSubclass) $ throwPositionalError
    (InvalidFunctionApplicationInvalidArgumentType expectedType exprType, annotationExpr expr)
  return annotatedExpr


checkForFieldRedeclaration :: Ident -> [FieldDef] -> ParseAnnotation -> TCM ()
checkForFieldRedeclaration classIdent_ classFields info =
  let repeatedFields = repeated $ map (\(FieldDef _ fIdent) -> fIdent) classFields in
  unless (null repeatedFields) $ throwPositionalError
    (RedeclarationClassField classIdent_ $ head repeatedFields, info)


topologicalSortTopDefs :: [(Ident, Type)] -> [(Ident, Type)]
topologicalSortTopDefs topDefs_ = sortBy ordering topDefs_
  where ordering (_, t1) (_, t2) = let defsMap = Map.fromList topDefs_ in
          if t1 == t2 then EQ else
            case (t1, t2) of
              (ClassType cIdent1 _ _ _, ClassType cIdent2 _ _ _) ->
                let tc1 = Class cIdent1; tc2 = Class cIdent2 in
                case (isSubclassOf_ defsMap tc1 tc2, isSubclassOf_ defsMap tc2 tc1) of
                  (True, False) -> GT
                  (False, True) -> LT
                  (False, False) -> EQ
                  (True, True) -> error "Two distinct classes are subclasses of each other?!"
              _ -> EQ


boolToLit :: Bool -> a -> ExprF a
boolToLit True a = ELitTrue a
boolToLit False a = ELitFalse a


isRetStmt :: StmtF a -> Bool
isRetStmt (VRet _) = True
isRetStmt (Ret _ _) = True
isRetStmt _ = False


getVarType :: Ident -> TCM (Maybe Type)
getVarType ident = do
  varInfoM <- getVarInfo ident
  return $ varType <$> varInfoM


getVarInfo :: Ident -> TCM (Maybe VariableInfo)
getVarInfo ident@(Ident sIdent) = do
  currentVars <- gets currentScope
  case Map.lookup ident currentVars of
    Just t -> return (Just t)
    Nothing -> do
                scopeVar <- gets ((>>= headMay) . Map.lookup ident . scopes)
                case scopeVar of
                  Just t -> return (Just t)
                  Nothing -> do
                              globals <- asks topDefs
                              case Map.lookup ident globals of
                                Just t -> return $ Just $ VariableInfo t (ST.mkSymbolName sIdent) False
                                Nothing -> return Nothing


clearCurrentScope :: TCM ()
clearCurrentScope = do
  st <- get
  put $ st { currentScope = Map.empty }


checkReturnCorrectnes :: BlockPar -> TCM ()
checkReturnCorrectnes (Block stmts a) =
  let retIndices = findIndices isRetStmt stmts in
  case retIndices of
    _:_:_ -> returnsError
    [n] -> when (n /= length stmts - 1) returnsError
    _ -> return ()
  where returnsError = throwError $ errWrapPos (BlockInvalidReturns, a)


isEmptyStmt :: StmtF a -> Bool
isEmptyStmt (Empty _) = True
isEmptyStmt _ = False


isSubclassOf :: Type -> Type -> TCM Bool
isSubclassOf t1 t2 = asks topDefs >>= \defs -> return $ isSubclassOf_ defs t1 t2


isSubclassOf_ :: Map Ident Type -> Type -> Type -> Bool
isSubclassOf_ defs (Class cIdent1) cType2@(Class cIdent2) =
  (cIdent1 == cIdent2) ||
  (case Map.lookup cIdent1 defs of
     Just (ClassType _ _ _ (Just cParent)) -> isSubclassOf_ defs (Class cParent) cType2
     _ -> False)
isSubclassOf_ _ _ _ = False

checkHasMain :: Map Ident Type -> TCM ()
checkHasMain topDefs_ =
  case Map.lookup (Ident "main") topDefs_ of
    Just (Fun Int []) -> return ()
    _ -> throwError $ LatteError $ GeneralError MainUndeclared


errWrapPos :: PositionalError -> LatteIssue
errWrapPos pos = LatteError $ PositionalError pos


throwPositionalError :: PositionalError -> TCM a
throwPositionalError = throwError . errWrapPos


takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []
