module Parser where

import System.IO
import Control.Monad
import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Test.QuickCheck
import Formula

languageDef =
   emptyDef { Token.commentStart    = "(-"
            , Token.commentEnd      = "-)"
            , Token.commentLine     = "--"
            , Token.identStart      = letter <|> oneOf "_"
            , Token.identLetter     = alphaNum <|> oneOf "_"
            , Token.reservedNames   = [ "Var"
                                      , "And"
                                      , "Not"
                                      , "Or"
                                      , "Implies"
                                      , "Iff"
                                      , "T"
                                      , "F"
                                      ]
            , Token.reservedOpNames = ["\""]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

formulaParser :: Parser Formula
formulaParser = whiteSpace >> formula

formula :: Parser Formula
formula = parens formula
    <|> trueFormula
    <|> falseFormula
    <|> varFormula
    <|> notFormula
    <|> andFormula
    <|> orFormula
    <|> impliesFormula
    <|> iffFormula

trueFormula :: Parser Formula
trueFormula = do reserved "T"
                 return T

falseFormula :: Parser Formula
falseFormula = do reserved "F"
                  return F

notFormula :: Parser Formula
notFormula = do
  reserved "Not"
  Not <$> formula

andFormula :: Parser Formula
andFormula = do
  reserved "And"
  phi <- formula
  And phi <$> formula
                
orFormula :: Parser Formula
orFormula = do
  reserved "Or"
  phi <- formula
  Or phi <$> formula

impliesFormula :: Parser Formula
impliesFormula = do
  reserved "Implies"
  phi <- formula
  Implies phi <$> formula

iffFormula :: Parser Formula
iffFormula = do
  reserved "Iff"
  phi <- formula
  Iff phi <$> formula

varFormula :: Parser Formula
varFormula = do reserved "Var"
                reservedOp "\"" 
                name <- identifier
                reservedOp "\""
                return $ Var name

parseString :: String -> Formula
parseString str =
    case parse formulaParser "" str of
        Left e  -> error $ show e
        Right r -> r

test :: Formula -> Bool
test phi = phi == parseString (show phi)

check = quickCheck (test hard_unsat_formula)