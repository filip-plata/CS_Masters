module Latte.Common.SymbolTable (
  SymbolTable,
  mkSymbolName,
  insertSymbol,
  resolveSymbol,

  getSymbolName,
  getSymbolType,
  getSymbolValue,

  freshSymbol,
  freshSymbolSuggestion,

  getClassSymbols,
  getClassFields,
  getClassMethods,
  symbolFieldOffset,
  symbolStorageSize,

  virtualMethodsTable,
  methodVTableOffset,
  getClassMethodType,

  empty,
  toList,

  SymbolValue(..),
  SymbolTableError(..),
  VTableMethodInfo(..),
  Symbol,
  SymbolName
) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Latte.Common.AST (Type(..), Ident(..))
import Data.Word (Word32)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (elemIndex, findIndex, find)

import Control.Monad

import Latte.Common.Runtime (arrayDefName)

data Symbol = Symbol SymbolName Type SymbolValue deriving (Eq, Show)
data SymbolValue = None | S String deriving (Eq, Show)
type SymbolName = String

type MethodVEntry = (SymbolName, Ident, Type)

data SymbolTableError =
  ClassParentNotDefined SymbolName String


data SymbolTable = SymbolTable {
  symbolMap :: Map SymbolName Symbol,
  classFields :: Map SymbolName [(Ident, Type)],
  classMethods :: Map SymbolName (Map Ident Type),
  classStorageSizes :: Map SymbolName Word32,
  vTables :: Map SymbolName ([] MethodVEntry),
  classes :: [SymbolName],
  overriddenClassMethods :: Map SymbolName (Set Ident)
} deriving (Eq, Show)

type NameProposal = (String, Int)


empty :: SymbolTable
empty = let eStr = "__empty_string" in SymbolTable {
  symbolMap = Map.fromList [(eStr, Symbol eStr Str (S ""))],
  classFields = Map.empty,
  classMethods = Map.empty,
  classStorageSizes = Map.empty,
  vTables = Map.empty,
  classes = [],
  overriddenClassMethods = Map.empty
}


-- | Inserts a new symbol into symbol table. Updates
-- all cached information about classes
insertSymbol :: SymbolName -> Type -> SymbolTable -> Either SymbolTableError SymbolTable
insertSymbol sName sType symTable = case sType of
  ClassType _ fields methods mParent ->
    let classStorageSize = sum $ map (storageSize . snd) fields in
    case mParent of
      Just (Ident parent) ->
          case resolveSymbol parent symTable of
            Just pSymbol -> let pSymName = getSymbolName pSymbol in
                            Right $
                              insertClassInfo classStorageSize fields methods mParent
                                (getClassFieldsUnsafe symTable pSymName)
                                (getClassMethodsUnsafe symTable pSymName)
                                (getClassStorageSizeUnsafe symTable pSymName)
                                insertSymbol'
            Nothing -> Left $ ClassParentNotDefined sName parent
      Nothing -> Right $ insertClassInfo
                          classStorageSize fields methods mParent
                          [] Map.empty neededStorage insertSymbol'
  _ -> Right insertSymbol'
  where insertSymbol' = symTable {
          symbolMap = Map.insert sName (Symbol sName sType None) $ symbolMap symTable
        }
        insertClassInfo classStorageSize fields methods mParent parentFields parentMethods parentSize symTable_ =
          let symT = calculateVTable symTable_ sName methods mParent in
          symT {
            classFields = Map.insert sName (reverse fields ++ parentFields) $ classFields symTable_,
            classStorageSizes = Map.insert sName (parentSize + classStorageSize) $ classStorageSizes symTable_,
            classes = sName:classes symTable_,
            classMethods = Map.insert sName (Map.union parentMethods $ Map.fromList methods) $ classMethods symTable_
          }
        storageSize t = case t of
                          Int -> 4
                          Bool -> 1
                          _ -> 8
        neededStorage = if sName == arrayDefName then 0 else 8 -- vtable pointer not needed for arrays


freshSymbol :: Type -> SymbolValue -> SymbolTable -> (SymbolTable, SymbolName)
freshSymbol = freshSymbolSuggestion ""


freshSymbolSuggestion :: String -> Type -> SymbolValue -> SymbolTable -> (SymbolTable, SymbolName)
freshSymbolSuggestion s t val symTable =
  let firstFreeNameProposal = while (isProposalTaken symTable) (\(ss, i) -> (ss, i + 1)) (s, (Map.size . symbolMap) symTable)
      symName = mkSymbolName $ proposalToString firstFreeNameProposal in
  (symTable { symbolMap = Map.insert symName (Symbol symName t val) $ symbolMap symTable }, symName)
  
  
symbolFieldOffset :: SymbolTable -> SymbolName -> Ident -> Maybe Integer
symbolFieldOffset symTable symbolName fieldName =
  case Map.lookup symbolName (classFields symTable) of
    Just fields -> case elemIndex fieldName (map fst fields) of
                     Just idx -> let res = Just $ toInteger $ length fields - idx in
                                 if symbolName == arrayDefName then (\x -> x - 1) <$> res else res
                     Nothing -> Nothing
    Nothing -> Nothing


virtualMethodsTable :: SymbolTable -> SymbolName -> Maybe [(SymbolName, Ident, Type)]
virtualMethodsTable symTable symName =
  reverse <$> Map.lookup symName (vTables symTable)


data VTableMethodInfo
  = VirtualOffset Integer
  | NonVirtualOrigin SymbolName
  | Unknown

-- | Returns index into virtual table. If it returns Nothing,
-- the method can be safely used directly, because
-- there is no virtual call possible.
methodVTableOffset :: SymbolTable -> SymbolName -> Ident -> VTableMethodInfo
methodVTableOffset symTable symName methodName = fromMaybe Unknown $ do
  vTable <- Map.lookup symName $ vTables symTable
  orig <- find (\(_, entryMethodName, _) -> entryMethodName == methodName) vTable
  let origClass = (\(orig_, _, _) -> orig_) orig
  idx <- findIndex (\(_, entryMethodName, _) -> entryMethodName == methodName) vTable
  let overriddenMethods = Map.findWithDefault Set.empty symName $ overriddenClassMethods symTable
  if Set.member methodName overriddenMethods
    then return $ VirtualOffset $ fromIntegral $ length vTable - idx - 1
    else return $ NonVirtualOrigin origClass


getClassSymbols :: SymbolTable -> [SymbolName]
getClassSymbols = classes


getClassFields :: SymbolTable -> SymbolName -> Maybe [(Ident, Type)]
getClassFields symTable symName = reverse <$> Map.lookup symName (classFields symTable)


getClassMethods :: SymbolTable -> SymbolName -> Maybe (Map Ident Type)
getClassMethods symTable symName = Map.lookup symName (classMethods symTable)


getClassMethodType :: SymbolTable -> SymbolName -> Ident -> Maybe Type
getClassMethodType symTable symName methodName =
  Map.lookup symName (classMethods symTable) >>=
  (Map.lookup methodName >=> (return . enrichFunctionTypeWithThis (Ident symName)))


getClassFieldsUnsafe :: SymbolTable -> SymbolName -> [(Ident, Type)]
getClassFieldsUnsafe symTable symName = fromJust $ getClassFields symTable symName


getClassMethodsUnsafe :: SymbolTable -> SymbolName -> Map Ident Type
getClassMethodsUnsafe symTable symName = fromJust $ getClassMethods symTable symName


getClassStorageSizeUnsafe :: SymbolTable -> SymbolName -> Word32
getClassStorageSizeUnsafe symTable symName = fromJust $ Map.lookup symName (classStorageSizes symTable)


symbolStorageSize :: SymbolTable -> SymbolName -> Maybe Word32
symbolStorageSize symTable symName = Map.lookup symName (classStorageSizes symTable)

-- | Calculates virtual table with standard algorithm
-- which rewrites rules for parent if necessary
calculateVTable :: SymbolTable -> SymbolName -> [(Ident, Type)] -> Maybe Ident -> SymbolTable
calculateVTable symTable classSym methods_ mParent =
  let vTable = vTables symTable
      methods = map (\(i, t) -> (i, enrichFunctionTypeWithThis (Ident classSym) t)) methods_
   in insertTables $ case mParent of
        Just (Ident parent) ->
          case Map.lookup (mkSymbolName parent) vTable of
            Just parentEntries ->
              let (rewrittenSubclass, newMethodNames, overwritten) =
                    foldl rewriteIfNeeded ([], Map.fromList methods, []) parentEntries
                  symTable_ = markAsOverridden symTable overwritten
               in (plainVTable (Map.toList newMethodNames) ++ reverse rewrittenSubclass, symTable_)
            Nothing ->
              error $ "No parent class definition " ++ parent ++ " in validated code?! Compiler internal error!"
        Nothing -> (plainVTable methods, symTable)
  where
    plainVTable = map (\(i, t) -> (classSym, i, t))
    rewriteIfNeeded (acc, methodMapping, overwritten) entry@(originalClassName, pMethodName, pType) =
      case Map.lookup pMethodName methodMapping of
        Just _ ->
          ( (classSym, pMethodName, swapThisType (Ident classSym) pType) : acc
          , Map.delete pMethodName methodMapping
          , (originalClassName, pMethodName) : overwritten -- rewrite the class method reference
           )
        Nothing -> (entry : acc, methodMapping, overwritten) -- no rewrite
    markAsOverridden =
      foldl
        (\symT (origClass, method) ->
           let ocm = overriddenClassMethods symT
               overriddenSet = Map.findWithDefault Set.empty origClass ocm
            in symT { overriddenClassMethods = Map.insert origClass (Set.insert method overriddenSet) ocm })
    insertTables (tables, symTable_) = symTable_ { vTables = Map.insert classSym tables $ vTables symTable_ }


enrichFunctionTypeWithThis :: Ident -> Type -> Type
enrichFunctionTypeWithThis classIdent (Fun rType argsT) = Fun rType $ Class classIdent:argsT
enrichFunctionTypeWithThis _ _ = error "Only method type can have 'this' appended"


swapThisType :: Ident -> Type -> Type
swapThisType classIdent (Fun rType argsT) = Fun rType $ Class classIdent : tail argsT
swapThisType _ _ = error "Swapping invalid types ?!"


resolveSymbol :: String -> SymbolTable -> Maybe Symbol
resolveSymbol s = Map.lookup (mkSymbolName s) . symbolMap 


getSymbolType :: Symbol -> Type
getSymbolType (Symbol _ t _) = t


getSymbolValue :: Symbol -> SymbolValue
getSymbolValue (Symbol _ _ v) = v


getSymbolName :: Symbol -> SymbolName
getSymbolName (Symbol n _ _ ) = n


toList :: SymbolTable -> [Symbol]
toList symT = map snd $ Map.toList $ symbolMap symT


mkSymbolName :: String -> SymbolName
mkSymbolName = id


isProposalTaken :: SymbolTable -> NameProposal -> Bool
isProposalTaken symT p = Map.member (mkSymbolName $ proposalToString p) $ symbolMap symT


proposalToString :: Show a => (String, a) -> String
proposalToString (s, i) = s ++ show i


while :: (a -> Bool) -> (a -> a) -> a -> a
while p = until (not . p)