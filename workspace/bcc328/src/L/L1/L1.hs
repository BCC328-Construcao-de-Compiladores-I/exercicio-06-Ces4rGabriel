import L.L1.Backend.CCodegen
import L.L1.Backend.V1Codegen
import L.L1.Interpreter.Interp
import L.L1.Frontend.Lexer 
import L.L1.Frontend.RecursiveParser
import L.L1.Frontend.LalrParser (parser)
import L.L1.Frontend.Syntax
import Utils.Pretty
import Utils.Repl
import Utils.Value
import V.V0.Instr

import System.Environment
import System.FilePath
import System.Process 
import Control.Monad (mapM_)

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts 


runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of 
  [Lexer file] ->
    alexBasedLexer file
  [Recursive file] -> 
    recursiveParser file
  [Lalr file] -> 
    lalrParser file 
  _ -> helpMessage


-- Implement the function to do lexical analysis for L1 programs

--alexBasedLexer :: FilePath -> IO ()
--alexBasedLexer file = error "Not implemtented!"

alexBasedLexer :: FilePath -> IO ()
alexBasedLexer file = do
  content <- readFile file
  let tokens = lexer content
  mapM_ printToken tokens


printToken :: Token -> IO ()
printToken (Token (l, c) lexeme) =
  putStrLn $ case lexeme of
    TAssign     -> "Atribuição := Linha:" ++ show l ++ " Coluna:" ++ show c
    TRead       -> "Palavra reservada read Linha:" ++ show l ++ " Coluna:" ++ show c
    TPrint      -> "Palavra reservada print Linha:" ++ show l ++ " Coluna:" ++ show c
    TPlus       -> "Operador + Linha:" ++ show l ++ " Coluna:" ++ show c
    TMinus      -> "Operador - Linha:" ++ show l ++ " Coluna:" ++ show c 
    TTimes      -> "Operador * Linha:" ++ show l ++ " Coluna:" ++ show c
    TLParen     -> "Parêntesis ( Linha:" ++ show l ++ " Coluna:" ++ show c
    TRParen     -> "Parêntesis ) Linha:" ++ show l ++ " Coluna:" ++ show c
    TSemicolon  -> "Ponto e vírgula ; Linha:" ++ show l ++ " Coluna:" ++ show c
    TComma      -> "Virgula , Linha:" ++ show l ++ " Coluna:" ++ show c
    TNumber n   -> "Número " ++ show n ++ " Linha:" ++ show l ++ " Coluna:" ++ show c
    TString s   -> "String \"" ++ s ++ "\" Linha:" ++ show l ++ " Coluna:" ++ show c 
    TId s       -> "Identificador " ++ s ++ " Linha:" ++ show l ++ " Coluna:" ++ show c
    TEOF        -> "EOF Linha:" ++ show l ++ " Coluna:" ++ show c



-- Implement the function to do syntax analysis using a recursive parser

-- recursiveParser :: FilePath -> IO ()
-- recursiveParser file = error "Not implemented!"


recursiveParser :: FilePath -> IO ()
recursiveParser file = do
  content <- readFile file
  case l1Parser content of
    Left err -> putStrLn $ "Erro de sintaxe:\n" ++ err
    Right ast -> do
      putStrLn "Análise sintática concluída"
      putStrLn "AST:"
      putStrLn (show (ppr ast))



lalrParser :: FilePath -> IO ()
lalrParser file = do
  content <- readFile file
  let tokens = lexer content
  let ast = parser tokens 
  putStrLn "Análise sintática (lalr) concluída"
  putStrLn "AST:"
  putStrLn (show (ppr ast))


-- help message


helpMessage :: IO ()
helpMessage 
  = putStrLn $ unlines [ "L1 language" 
                       , "Usage: l1 [--lexer-only | --recursive | --help]"
                       , "--lexer-only: does the lexical analysis of the input programming using a Alex based lexer."
                       , "--recursive: does the syntax analysis using a recursive descendent Megaparsec parser."
                       , "--lalr: does the syntax analysis using a lalr parser."
                       , "--help: prints this help message."
                       ]

-- parse command line arguments 


data Option 
  = Help 
  | Lexer FilePath
  | Recursive FilePath
  | Lalr FilePath 
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args = 
  case args of 
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--recursive" : arg : _) -> [Recursive arg]
    ("--lalr" : arg : _) -> [Lalr arg]
    _ -> [Help]
