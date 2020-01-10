module Latte.Frontend.Parser where
{-
(
    ParseErrorType
  , ParseStreamType
  , parseProgram
)
where
-}

import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Data.Maybe (mapMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Text.Megaparsec.Debug

import Latte.Common.AST
import Latte.Common.Annotation


type ParseErrorType = Void
type ParseStreamType = String


type Parser = Parsec ParseErrorType ParseStreamType


parseProgram :: String -> ParseStreamType -> Either (ParseErrorBundle ParseStreamType ParseErrorType) ProgramPar
parseProgram = parse programParser


programParser :: Parser ProgramPar
programParser = do
  start <- getSourcePos
  topDefs <- between sc eof $ some topDefParser
  Program topDefs . mkParseAnnotation start <$> getSourcePos


topDefParser :: Parser TopDefPar
topDefParser = choice [classDefParser, globalFunctionDefParser]

classDefParser = do
  start <- getSourcePos
  rword "class"
  className <- identifier
  parentClassM <- option Nothing (Just <$> (rword "extends" >> identifier))
  decls <- between (symbol "{") (symbol "}") $ many classDeclParser
  let fields = mapMaybe declField decls
  let methods = mapMaybe declMethods decls
  ClassDef className parentClassM fields methods . mkParseAnnotation start <$> getSourcePos
  where declField decl = case decl of
                          Field f -> Just f
                          _       -> Nothing
        declMethods decl = case decl of
                             MethodDef f -> Just f
                             _           -> Nothing


classDeclParser :: Parser ClassDeclPar
classDeclParser = choice [try classMethodParser, classFieldParser]

classFieldParser :: Parser ClassDeclPar
classFieldParser = do
  t <- typeParser
  ident <- identifier
  semi
  return $ Field $ FieldDef t ident


classMethodParser :: Parser ClassDeclPar
classMethodParser = MethodDef <$> functionDefParser



globalFunctionDefParser :: Parser TopDefPar
globalFunctionDefParser = GlobalFunDef <$> functionDefParser
  

functionDefParser :: Parser FunDefPar
functionDefParser = do
  start <- getSourcePos
  rType <- typeParser
  fName <- identifier
  args <- parens $ sepBy argParser comma
  block <- blockParser
  FunctionDef rType fName args block . mkParseAnnotation start <$> getSourcePos

blockParser :: Parser BlockPar
blockParser = do
  start <- getSourcePos
  stmts <- between (symbol "{") (symbol "}") $ many stmtParser
  Block stmts . mkParseAnnotation start <$> getSourcePos


stmtParser :: Parser StmtPar
stmtParser =
    choice
       [ stmtWhileParser
       , stmtForParser
       , stmtEmptyParser
       , stmtBlockParser
       , try stmtDeclParser
       , try stmtRetParser
       , stmtRetVoidParser
       , try stmtCondElseParser
       , stmtCondParser
       , try stmtAssignmentParser
       , try stmtDecrParser
       , try stmtIncrParser
       , stmtExprParser
       ] <?> "expected latte statement"


stmtEmptyParser :: Parser StmtPar
stmtEmptyParser = do
  start <- getSourcePos
  semi
  Empty . mkParseAnnotation start <$> getSourcePos
stmtBlockParser = do
  start <- getSourcePos
  block <- blockParser
  BStmt block . mkParseAnnotation start <$> getSourcePos
stmtDeclParser = do
  start <- getSourcePos
  declType <- typeParser
  items <- sepBy1 itemParser comma
  semi
  Decl declType items . mkParseAnnotation start <$> getSourcePos
stmtAssignmentParser = do
  start <- getSourcePos
  lvalue <- exprParser
  symbol "="
  rvalue <- exprParser
  semi
  Ass lvalue rvalue . mkParseAnnotation start <$> getSourcePos
stmtIncrParser = do
  start <- getSourcePos
  ident <- identifier
  symbol "++"
  semi
  Incr ident . mkParseAnnotation start <$> getSourcePos
stmtDecrParser = do
  start <- getSourcePos
  ident <- identifier
  symbol "--"
  semi
  Decr ident . mkParseAnnotation start <$> getSourcePos
stmtRetParser = do
  start <- getSourcePos
  rword "return"
  expr <- exprParser
  semi
  Ret expr . mkParseAnnotation start <$> getSourcePos
stmtRetVoidParser = do
  start <- getSourcePos
  rword "return"
  semi
  VRet . mkParseAnnotation start <$> getSourcePos
stmtCondParser = do
  start <- getSourcePos
  expr <- ifStartParser
  stmt <- stmtParser
  Cond expr stmt . mkParseAnnotation start <$> getSourcePos
stmtCondElseParser = do
  start <- getSourcePos
  expr <- ifStartParser
  stmtTrue <- stmtParser
  rword "else"
  stmtFalse <- stmtParser
  CondElse expr stmtTrue stmtFalse . mkParseAnnotation start <$> getSourcePos
stmtWhileParser = do
  start <- getSourcePos
  rword "while"
  expr <- parens exprParser
  stmt <- stmtParser
  While expr stmt . mkParseAnnotation start <$> getSourcePos
stmtForParser = do
  start <- getSourcePos
  rword "for"
  symbol "("
  arg <- argParser
  symbol ":"
  expr <- exprParser
  symbol ")"
  stmt <- stmtParser
  For arg expr stmt . mkParseAnnotation start <$> getSourcePos


ifStartParser :: Parser ExprPar
ifStartParser = rword "if" >> parens exprParser
  
  
stmtExprParser = do
  start <- getSourcePos
  expr <- exprParser
  symbol ";"
  SExp expr . mkParseAnnotation start <$> getSourcePos


exprParser :: Parser ExprPar
exprParser = makeExprParser exprTermParser exprOperators

exprOperators :: [[Operator Parser ExprPar]]
exprOperators = [ [methodCallOperator]
                , [ dotOperator, subscriptOperator ]
                , [ prefix "-" (\expr -> Neg expr $ annotationExpr expr)
                  , prefix "!" (\expr -> Not expr $ annotationExpr expr) ]
                  
                , [ binaryL "*" (\expr1 expr2 -> EMul expr1 Times expr2 $ mergeParseAnnotation (annotationExpr expr1) (annotationExpr expr2))
                  , binaryL "/" (\expr1 expr2 -> EMul expr1 Div expr2 $ mergeParseAnnotation (annotationExpr expr1) (annotationExpr expr2))
                  , binaryL "%" (\expr1 expr2 -> EMul expr1 Mod expr2 $ mergeParseAnnotation (annotationExpr expr1) (annotationExpr expr2)) ]
                  
                , [ binaryL "+" (\expr1 expr2 -> EAdd expr1 Plus expr2 $ mergeParseAnnotation (annotationExpr expr1) (annotationExpr expr2))
                  , binaryL "-" (\expr1 expr2 -> EAdd expr1 Minus expr2 $ mergeParseAnnotation (annotationExpr expr1) (annotationExpr expr2)) ]
                  
                , [ binaryR "<=" (\expr1 expr2 -> ERel expr1 LE expr2 $ mergeParseAnnotation (annotationExpr expr1) (annotationExpr expr2))
                  , binaryR "<" (\expr1 expr2 -> ERel expr1 LTH expr2 $ mergeParseAnnotation (annotationExpr expr1) (annotationExpr expr2))
                  , binaryR ">=" (\expr1 expr2 -> ERel expr1 GE expr2 $ mergeParseAnnotation (annotationExpr expr1) (annotationExpr expr2))
                  , binaryR ">" (\expr1 expr2 -> ERel expr1 GTH expr2 $ mergeParseAnnotation (annotationExpr expr1) (annotationExpr expr2))
                  , binaryR "==" (\expr1 expr2 -> ERel expr1 EQU expr2 $ mergeParseAnnotation (annotationExpr expr1) (annotationExpr expr2))
                  , binaryR "!=" (\expr1 expr2 -> ERel expr1 NE expr2 $ mergeParseAnnotation (annotationExpr expr1) (annotationExpr expr2)) ]
                  
                , [ binaryR "&&" (\expr1 expr2 -> EAnd expr1 expr2 $ mergeParseAnnotation (annotationExpr expr1) (annotationExpr expr2)) ]
                
                , [ binaryR "||" (\expr1 expr2 -> EOr expr1 expr2 $ mergeParseAnnotation (annotationExpr expr1) (annotationExpr expr2)) ]
                ]

exprTermParser :: Parser ExprPar
exprTermParser = choice [ exprLitIntParser, exprLitTrueParser, exprLitFalseParser,
                          exprStringParser,
                          try exprNewArray,
                          exprNewObject,
                          exprSelf,
                          try exprCall,
                          exprVarParser,
                          try exprNull,
                          parens exprParser ]
                          <?>
                          "expected terminal expression: integer literal, boolean literal, \
                          \ string literal, function application, identifier or expression \
                          \ in parenthesis"

exprVarParser = do
  start <- getSourcePos
  ident <- identifier
  EVar ident . mkParseAnnotation start <$> getSourcePos
exprLitIntParser = do
  start <- getSourcePos
  i <- integer
  ELitInt i . mkParseAnnotation start <$> getSourcePos
exprLitTrueParser = do
  start <- getSourcePos
  rword "true"
  ELitTrue . mkParseAnnotation start <$> getSourcePos
exprLitFalseParser = do
  start <- getSourcePos
  rword "false"
  ELitFalse . mkParseAnnotation start <$> getSourcePos
exprStringParser = do
  start <- getSourcePos
  s <- stringLiteral
  EString s . mkParseAnnotation start <$> getSourcePos
exprNewArray = do
  start <- getSourcePos
  rword "new"
  t <- typePlainParser
  init <- between (symbol "[") (symbol "]") exprParser
  EArrayNew t init . mkParseAnnotation start <$> getSourcePos
exprNewObject = do
  start <- getSourcePos
  rword "new"
  ident <- identifier
  EObjNew ident . mkParseAnnotation start <$> getSourcePos
exprNull = do
  start <- getSourcePos
  classType <- parens typeClassName
  rword "null"
  ENull classType . mkParseAnnotation start <$> getSourcePos
exprSelf = do
  start <- getSourcePos
  rword "self"
  ESelf . mkParseAnnotation start <$> getSourcePos
exprCall = do
  start <- getSourcePos
  var <- exprVarParser
  args <- parens $ sepBy exprParser comma
  EApp var args . mkParseAnnotation start <$> getSourcePos


itemParser :: Parser ItemPar
itemParser = choice [try itemInitParser, itemNoInitParser] <?> "expected variable definition"
itemInitParser = do
  start <- getSourcePos
  ident <- identifier
  symbol "="
  expr <- exprParser
  Init ident expr . mkParseAnnotation start <$> getSourcePos
itemNoInitParser = do
  start <- getSourcePos
  ident <- identifier
  NoInit ident . mkParseAnnotation start <$> getSourcePos


argParser :: Parser ArgPar
argParser = do
  start <- getSourcePos
  argType <- typeParser
  ident <- identifier
  Arg argType ident . mkParseAnnotation start <$> getSourcePos


typeParser :: Parser Type
typeParser = choice [try typeArrayParser, typePlainParser]
typePlainParser = choice [typeBooleanParser, typeStringParser,
                          typeVoidParser, typeIntParser, typeClassName]


typeClassName = Class <$> identifier
typeArrayParser = do
  t <- typePlainParser
  symbol "[" >> symbol "]"
  return $ Array t
typeIntParser = rword "int" >> return Int
typeBooleanParser = rword "boolean" >> return Bool
typeStringParser = rword "string" >> return Str
typeVoidParser = rword "void" >> return Latte.Common.AST.Void


binaryL, binaryR :: String -> (ExprPar -> ExprPar -> ExprPar) -> Operator Parser ExprPar
binaryL  name f = InfixL  (f <$ symbol name)
binaryR  name f = InfixR  (f <$ symbol name)

prefix, postfix :: String -> (ExprPar -> ExprPar) -> Operator Parser ExprPar
prefix  name f = Prefix $ foldr1 (.) <$> some (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

dotOperator = Postfix (fmap (\ident expr -> EAttrRef expr ident $ annotationExpr expr)
                      (symbol "." >> identifier))
methodCallOperator = Postfix (fmap (\(ident, args) expr -> EApp (EAttrRef expr ident $ annotationExpr expr) args $ annotationExpr expr)
                              (try $ do
                                 symbol "."
                                 ident <- identifier
                                 args <- parens $ sepBy exprParser comma
                                 return (ident, args)
                                 ))
subscriptOperator = Postfix (fmap (\exprSub expr -> ESubscript expr exprSub $ mergeParseAnnotation (annotationExpr expr) (annotationExpr exprSub))
                            (subscript exprParser))
callOperator = Postfix (fmap (\args expr -> EApp expr args $ annotationExpr expr)
                              (parens $ sepBy exprParser comma))


integer :: Parser Integer
integer = lexeme L.decimal


stringLiteral :: Parser String
stringLiteral = lexeme $ char '\"' *> manyTill L.charLiteral (char '\"')


identifier :: Parser Ident
identifier = Ident <$> (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x


rws :: [String] -- list of reserved words
rws = ["if", "else", "while", "int", "string", "null",
       "boolean", "void", "return", "true", "false",
       "class", "extends", "new", "for", "self",
       "__malloc", "__concat_str"]


rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)


semi :: Parser String
semi = symbol ";"


comma :: Parser String
comma = symbol ","


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


subscript :: Parser a -> Parser a
subscript = between (symbol "[") (symbol "]")


symbol :: String -> Parser String
symbol = L.symbol sc


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#" <|> L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"
