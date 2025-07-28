{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module L.L2.Frontend.Lexer (Token (..), Lexeme (..), lexer) where
}

%wrapper "posn"

$digit = 0-9           
$alpha = [a-zA-Z]
$char = [^"\n] -- qualquer caractere exceto aspas duplas e nova linha

-- second RE macros

@number     = $digit+
@id         = $alpha ($alpha | $digit)*
@string     = \" $char* \"

-- tokens declarations

tokens :-
      $white+       ;
      "//" .* ;
      @number       {mkNumber}
      @string       {mkString}
      "("           {simpleToken TLParen}
      ")"           {simpleToken TRParen}
      "+"           {simpleToken TPlus}
      "*"           {simpleToken TTimes}
      "/"           {simpleToken TDiv}
      "read"        {simpleToken TRead}
      "print"       {simpleToken TPrint}
      ";"           {simpleToken TSemicolon}
      ","           {simpleToken TComma}
      ":="          {simpleToken TAssign}
      "-"           {simpleToken TMinus}
      -- Novas palavras-chave de L2 (antes de @id)
      "def"         {simpleToken TDef}
      "in"          {simpleToken TIn}
      "end"         {simpleToken TEnd}
      @id           {mkId}

{
data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme
    } deriving (Eq, Ord, Show)

data Lexeme
  = TNumber Int
  | TLParen
  | TRParen
  | TPlus
  | TTimes
  | TEOF
  | TRead
  | TPrint
  | TSemicolon
  | TComma
  | TAssign
  | TString String
  | TId String
  | TMinus
  | TDiv
  -- Novos lexemas para L2
  | TDef
  | TIn
  | TEnd
  deriving (Eq, Ord, Show)

position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

mkNumber :: AlexPosn -> String -> Token
mkNumber p s = Token (position p) (TNumber $ read s)

mkString :: AlexPosn -> String -> Token
mkString p s = Token (position p) (TString $ init $ tail s)

mkId :: AlexPosn -> String -> Token
mkId p s = Token (position p) (TId s)

simpleToken :: Lexeme -> AlexPosn -> String -> Token
simpleToken lx p _ = Token (position p) lx

lexer :: String -> [Token]
lexer = alexScanTokens
}