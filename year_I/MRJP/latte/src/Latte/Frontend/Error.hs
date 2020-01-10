module Latte.Frontend.Error where

import Text.Printf (printf)
import Text.Megaparsec.Pos (sourcePosPretty)

import Latte.Common.Annotation
import Latte.Common.AST (Ident(..), Type(..))

data LatteIssue = LatteError LatteError | LatteWarning LatteWarning

data LatteError = PositionalError PositionalError | GeneralError GeneralError
data LatteWarning = None


data GeneralError
  = MainUndeclared
  | ParentClassUndefined Ident

data TypeErrorRaw
  = UndefinedIdentifier Ident
  | VariableRedeclaration [Ident]
  | InternalCompilerError String
  | DivisionByZero
  | ExpressionBinaryInvalid Type Type [Type]
  | ExpressionNegationInvalid Type
  | ExpressionInvalidN Type
  | ExpressionAddInvalid Type Type
  | ExpectedBooleanConditional
  | AssignmentInvalid Ident Type Type
  | RetOutsideOfFunction
  | RetVoidExpr
  | RetInvalidType Type Type
  | InvalidFunctionApplicationInvalidArgumentType Type Type
  | InvalidFunctionApplicationArgumentCount Int Int
  | CallingUnfunctionObject
  | UnexpectedIdentifierType Type Type Ident
  | NonVoidFunctionMayNotReturn Ident
  | InvalidForeach Type
  | InvalidAttribute Type Ident
  | AssignmentTypeMismatch Type Type
  | NotAnLvalue
  | InvalidArrayInitialization
  | ArraySubscriptNotAnInt
  | SubscriptExpectedArray Type
  | BlockInvalidReturns
  | RedeclarationFunction Ident
  | RedeclarationClass Ident
  | RedeclarationClassField Ident Ident
  | UndefinedClass Ident
  | SelfOutsideMethod
  | VoidTypeDeclaration Ident
  | IntegerLiteralOutsideOfRange Integer
  | ExpressionEqualityInvalid Type Type

type TypeErrorInfo = ParseAnnotation
type PositionalError = (TypeErrorRaw, TypeErrorInfo)


latteIssuePretty :: LatteIssue -> String
latteIssuePretty (LatteWarning warn) = latteWarningPretty warn
latteIssuePretty (LatteError err) = latteErrorPretty err


latteWarningPretty :: LatteWarning -> String
latteWarningPretty warn = "Warning: " ++
  case warn of
    None -> ""


latteErrorPretty :: LatteError -> String
latteErrorPretty err = "Error: " ++
  case err of
    GeneralError e -> generalErrorPretty e
    PositionalError e -> typeErrorPretty e


generalErrorPretty :: GeneralError -> String
generalErrorPretty MainUndeclared = "Main function must be present and have proper type"
generalErrorPretty (ParentClassUndefined (Ident className)) =
  printf "Required parent class '%s' is not defined in source file" className


typeErrorPretty :: PositionalError -> String
typeErrorPretty (typeErrorRaw, info) = typeErrorContext info ++ " " ++ typeErrorRawPretty typeErrorRaw

typeErrorRawPretty :: TypeErrorRaw -> String
typeErrorRawPretty (UndefinedIdentifier (Ident ident)) =
  printf "Undefined identifier: %s" ident
typeErrorRawPretty (VariableRedeclaration idents) =
  printf "Redeclaration of variables: " ++ unwords (map (\(Ident ident) -> printf "'%s'" ident) idents)
typeErrorRawPretty (InternalCompilerError moreInfo) =
  printf "FATAL ERROR: %s\n" moreInfo
typeErrorRawPretty DivisionByZero = "Effective division by zero"
typeErrorRawPretty (ExpressionBinaryInvalid typeExpr1 typeExpr2 expectedTypes) =
  printf "Invalid binary expression operands types: %s and %s, while both were expected to have on of types: '%s'"
          (show typeExpr1) (show typeExpr2) (show expectedTypes)
typeErrorRawPretty (ExpressionNegationInvalid realType) =
  printf "Expected %s type in numerical negation expression instead of %s"
          (show Int) (show realType)
typeErrorRawPretty (ExpressionInvalidN realType) =
  printf "Expected %s type in boolean negation expression instead of %s"
          (show Bool) (show realType)
typeErrorRawPretty (ExpressionAddInvalid exprType1 exprType2) =
  printf "Only two 'int's or 'string's can be added and 'int's subtracted, but %s and %s were given"
          (show exprType1) (show exprType2)
typeErrorRawPretty ExpectedBooleanConditional =
  printf "Expected boolean inside condition"
typeErrorRawPretty (AssignmentInvalid (Ident ident) exprType foundType) =
  printf "In assignment to '%s' expected expression to be of type '%s', but '%s' found"
          ident (show foundType) (show exprType)
typeErrorRawPretty RetOutsideOfFunction =
  printf "Return is allowed only inside a function"
typeErrorRawPretty (RetInvalidType functionReturnType returnedType) =
  printf "Returning from function type '%s', but type '%s' is expected"
          (show returnedType) (show functionReturnType)
typeErrorRawPretty (InvalidFunctionApplicationInvalidArgumentType expectedType foundType) =
  printf "Type mismatch in function argument, expected type '%s', but '%s' found"
          (show expectedType) (show foundType)
typeErrorRawPretty (InvalidFunctionApplicationArgumentCount expected given) =
  printf "Expected '%d' arguments when calling function, but '%d' were given"
          expected given
typeErrorRawPretty CallingUnfunctionObject =
  printf "Not a function and cannot be called"
typeErrorRawPretty (UnexpectedIdentifierType expectedType foundType (Ident ident)) =
  printf "Expected identifier '%s' to have type '%s', but '%s' was found"
          ident (show expectedType) (show foundType)
typeErrorRawPretty (NonVoidFunctionMayNotReturn (Ident ident)) = printf "Function '%s' marked non-void may not return" ident
typeErrorRawPretty (InvalidForeach iterType) =
  printf "Iterated expression has to be of type 'parent-class-of %s []', because iterator has type %s"
  (show iterType) (show iterType)
typeErrorRawPretty (InvalidAttribute t (Ident attr)) =
  printf "Type '%s' has no attribute %s" (show t) attr
typeErrorRawPretty (AssignmentTypeMismatch lType rType) =
  printf "Type mismatch in assignment - on the left '%s', on the right '%s'" (show lType) (show rType)
typeErrorRawPretty NotAnLvalue = "Left hand side expression is not an lvalue"
typeErrorRawPretty InvalidArrayInitialization = "Array length initilizer must have type 'int'"
typeErrorRawPretty ArraySubscriptNotAnInt = "Index to array must have type 'int'"
typeErrorRawPretty (SubscriptExpectedArray t) =
  printf "Tried to subscript type '%s', but only arrays can be indexed" (show t)
typeErrorRawPretty BlockInvalidReturns = "Block of statements may have at most one return at the end of block"
typeErrorRawPretty (RedeclarationFunction (Ident fName)) =
  printf "Redeclaration of a function with name '%s'" fName
typeErrorRawPretty (RedeclarationClass (Ident cName)) =
  printf "Redeclaration of a class with name '%s'" cName
typeErrorRawPretty (RedeclarationClassField (Ident cName) (Ident fieldName)) =
  printf "Redeclaration of the field '%s' in '%s' class" fieldName cName
typeErrorRawPretty (UndefinedClass (Ident cName)) =
  printf "Undefined class '%s'" cName
typeErrorRawPretty SelfOutsideMethod = "Self outside of method definition"
typeErrorRawPretty (VoidTypeDeclaration (Ident ident)) =
  printf "Cannot declare variable '%s' with type Void" ident
typeErrorRawPretty RetVoidExpr = "Cannot return void type expression"
typeErrorRawPretty (IntegerLiteralOutsideOfRange integer) =
  printf "Integer '%s' outside of range of valid integer literals" (show integer)
typeErrorRawPretty (ExpressionEqualityInvalid t1 t2) =
  printf "Any kind of equality testing can only be done on covariant types, but '%s' and '%s' were given"
         (show t1) (show t2)


typeErrorContext :: TypeErrorInfo -> String
typeErrorContext (ParseAnnotation start_ _) = sourcePosPretty start_
