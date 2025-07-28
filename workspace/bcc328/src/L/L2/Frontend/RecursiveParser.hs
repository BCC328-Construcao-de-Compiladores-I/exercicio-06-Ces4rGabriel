module L.L2.Frontend.RecursiveParser where
import Control.Applicative hiding (many)
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import L.L2.Frontend.Syntax (E2(..), L2(..), S2(..))
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


pVarExpr :: Parser E2
pVarExpr = (LVar . Var) <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser E2
pInteger = (LVal . VInt) <$> lexeme L.decimal

pStringExpr :: Parser E2
pStringExpr = (LVal . VStr) <$> pStringLiteral

pTerm :: Parser E2
pTerm = choice
  [ parens pExpr
  , pVarExpr
  , pInteger
  , pStringExpr
  ]

operatorTable :: [[Operator Parser E2]]
operatorTable =
  [ [ InfixL (LMul <$ symbol "*")
    , InfixL (LDiv <$ symbol "/")
    ]
  , [ InfixL (LAdd <$ symbol "+")
    , InfixL (LMinus <$ symbol "-")
    ]
  ]

pExpr :: Parser E2
pExpr = makeExprParser pTerm operatorTable


pRead :: Parser S2
pRead = symbol "read" *> parens (LRead <$> pStringLiteral <* symbol "," <*> pVar)

pPrint :: Parser S2
pPrint = LPrint <$> (symbol "print" *> parens pExpr)

pAssign :: Parser S2
pAssign = LAssign <$> pVar <* symbol ":=" <*> pExpr

pDef :: Parser S2
pDef = do
  _ <- symbol "def"
  v <- pVar
  _ <- symbol ":="
  e <- pExpr
  _ <- symbol "in"
  ss <- pStatements
  _ <- symbol "end"
  return (Def v e ss)

-- pStatement agora inclui o pDef
pStatement :: Parser S2
pStatement = choice
  [ try pRead
  , try pPrint
  , try pAssign
  , pDef 
  ]

pStatements :: Parser [S2]
pStatements = pStatement `sepEndBy` symbol ";"


program :: Parser L2
program = L2 <$> pStatements

l2Parser :: String -> Either String L2
l2Parser input =
  case parse (slexer *> program <* eof) "" input of
    Left err -> Left $ errorBundlePretty err
    Right prog -> Right prog