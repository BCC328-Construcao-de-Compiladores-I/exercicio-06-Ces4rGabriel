module L.L1.Frontend.RecursiveParser where

import Control.Applicative hiding (many)
import Control.Monad.Combinators.Expr
import Data.Void (Void)

import L.L1.Frontend.Syntax (E1(..), L1(..), S1(..))
import Utils.Value (Value(..))
import Utils.Var (Var(..))

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

slexer :: Parser ()
slexer = L.space space1
                 (L.skipLineComment "//")
                 (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme slexer

symbol :: String -> Parser String
symbol s = L.symbol slexer s

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")



pVar :: Parser Var
pVar = Var <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable name")

pStringLiteral :: Parser String
pStringLiteral = lexeme (char '"' *> manyTill L.charLiteral (char '"'))


pVarExpr :: Parser E1
pVarExpr = (LVar . Var) <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser E1
pInteger = (LVal . VInt) <$> lexeme L.decimal

pStringExpr :: Parser E1
pStringExpr = (LVal . VStr) <$> pStringLiteral

pTerm :: Parser E1
pTerm = choice
  [ parens pExpr
  , pVarExpr
  , pInteger
  , pStringExpr
  ]

operatorTable :: [[Operator Parser E1]]
operatorTable =
  [ [ InfixL (LMul <$ symbol "*")
    , InfixL (LDiv <$ symbol "/")
    ]
  , [ InfixL (LAdd <$ symbol "+")
    , InfixL (LMinus <$ symbol "-")
    ]
  ]

pExpr :: Parser E1
pExpr = makeExprParser pTerm operatorTable



pRead :: Parser S1
pRead = symbol "read" *> parens (LRead <$> pStringLiteral <* symbol "," <*> pVar)

pPrint :: Parser S1
pPrint = LPrint <$> (symbol "print" *> parens pExpr)

pAssign :: Parser S1
pAssign = LAssign <$> pVar <* symbol ":=" <*> pExpr

pStatement :: Parser S1
pStatement = choice
  [ try pRead
  , try pPrint
  , pAssign
  ]


program :: Parser L1
program = L1 <$> pStatement `sepEndBy` symbol ";"

l1Parser :: String -> Either String L1
l1Parser input =
  case parse (slexer *> program <* eof) "" input of
    Left err -> Left $ errorBundlePretty err
    Right prog -> Right prog
