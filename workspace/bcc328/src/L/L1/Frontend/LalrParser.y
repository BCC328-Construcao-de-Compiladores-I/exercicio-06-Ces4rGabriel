{
module L.L1.Frontend.LalrParser where

import L.L1.Frontend.Lexer (Token(..), Lexeme(..))
import L.L1.Frontend.Syntax (L1(..), S1(..), E1(..))
import Utils.Var (Var(..))
import Utils.Value (Value(..))
import Control.Monad (void)
}

%name parser
%tokentype { Token }
%error { parseError }

%left '+' '-'
%left '*' '/'

%token
    var       { Token _ (TId $$) }
    num       { Token _ (TNumber $$) }
    str       { Token _ (TString $$) }
    ":="      { Token _ TAssign }
    "read"    { Token _ TRead }
    "print"   { Token _ TPrint }
    '('       { Token _ TLParen }
    ')'       { Token _ TRParen }
    ','       { Token _ TComma }
    ';'       { Token _ TSemicolon }
    '/'       { Token _ TDiv}
    '+'       { Token _ TPlus }
    '-'       { Token _ TMinus }
    '*'       { Token _ TTimes }

%%

L1 : Stmt L1    { L1 ($1 : let (L1 ss) = $2 in ss) }
   |            { L1 [] }

Stmt : var ":=" Expr ';'               { LAssign (Var $1) $3 }
     | "read" '(' str ',' var ')' ';'  { LRead $3 (Var $5) }
     | "print" '(' Expr ')' ';'        { LPrint $3 }

Expr : Expr '+' Term   { LAdd $1 $3 }
     | Expr '-' Term   { LMinus $1 $3 }
     | Term            { $1 }

Term : Term '*' Factor { LMul $1 $3 }
     | Term '/' Factor { LDiv $1 $3 }
     | Factor          { $1 }

Factor : Atom          { $1 }

Atom : num             { LVal (VInt $1) }
     | var             { LVar (Var $1) }
     | str             { LVal (VStr $1) }
     | '(' Expr ')'    { $2 }

{
parseError :: [Token] -> a
parseError [] = error "Erro de sintaxe no final do arquivo."
parseError (t:_) = -- Pega o primeiro token 't' da lista
    let (Token (line, col) lexeme) = t
    in error $ "Erro de sintaxe na linha " ++ show line ++ ", coluna " ++ show col ++ " perto de " ++ show lexeme
}
