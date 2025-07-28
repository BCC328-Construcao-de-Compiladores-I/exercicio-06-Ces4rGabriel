module Main where

import L.L2.Frontend.Lexer
import L.L2.Frontend.Syntax
import L.L2.Frontend.RecursiveParser (l2Parser)
import L.L2.Interpreter.Interp (interpL2) 
import L.L2.Frontend.TypeCheck (typeCheck) 
import L.L2.Backend.V1Codegen (v1gen) 
import L.L2.Backend.CCodegen (cgen) 

import Utils.Pretty

import System.Environment
import System.FilePath
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..)) 
import Control.Monad (when)

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts

data Option
  = Help
  | Lexer FilePath
  | Parser FilePath
  | Interpret FilePath
  | V1Compile FilePath
  | CCompile FilePath
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args =
  case args of
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--parse-only" : arg : _) -> [Parser arg]
    ("--interpret" : arg : _)  -> [Interpret arg]
    ("--v1-compile" : arg : _) -> [V1Compile arg] -- NOVA OPÇÃO
    ("--c-compile" : arg : _)  -> [CCompile arg]  -- NOVA OPÇÃO
    _ -> [Help]

runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of
  [Lexer file]       -> lexerOnly file
  [Parser file]      -> parserOnly file
  [Interpret file]   -> interpretOnly file
  [V1Compile file]   -> v1Compiler file
  [CCompile file]    -> cCompiler file
  _                  -> helpMessage

runAnalysisPipeline :: FilePath -> IO (Either String L2)
runAnalysisPipeline file = do
    content <- readFile file
    case l2Parser content of
        Left err -> return $ Left ("Erro de Sintaxe:\n" ++ err)
        Right ast ->
            case typeCheck ast of
                Left err -> return $ Left ("Erro Semantico:\n" ++ err)
                Right typedAst -> return $ Right typedAst

interpretOnly :: FilePath -> IO ()
interpretOnly file = do
  result <- runAnalysisPipeline file
  case result of
    Left err -> putStrLn err
    Right ast -> do
      putStrLn "Programa verificado com sucesso. Iniciando interpretacao..."
      interpL2 ast

v1Compiler :: FilePath -> IO ()
v1Compiler file = do
  result <- runAnalysisPipeline file
  case result of
    Left err -> putStrLn err
    Right ast -> do
      putStrLn "Gerando codigo para a maquina V1..."
      let instructions = v1gen ast
      let outFile = replaceExtension file ".v1"
      writeFile outFile (unlines (map show instructions))
      putStrLn $ "Codigo V1 salvo em: " ++ outFile

cCompiler :: FilePath -> IO ()
cCompiler file = do
  result <- runAnalysisPipeline file
  case result of
    Left err -> putStrLn err
    Right ast -> do
      putStrLn "Gerando codigo C..."
      let cCode = cgen ast
      let cFile = replaceExtension file ".c"
      let outFile = dropExtension file
      writeFile cFile cCode
      putStrLn $ "Codigo C salvo em: " ++ cFile
      putStrLn "Compilando com GCC..."
      (exitCode, stdout, stderr) <- readProcessWithExitCode "gcc" [cFile, "-o", outFile] ""
      case exitCode of
        ExitSuccess -> putStrLn $ "Compilacao bem-sucedida. Executavel salvo em: " ++ outFile
        ExitFailure _ -> do
          putStrLn "Erro na compilacao com GCC:"
          putStrLn stdout
          putStrLn stderr

helpMessage :: IO ()
helpMessage = putStrLn $ unlines [
    "L2 language",
    "Uso: l2 [opcao] [arquivo]",
    "Opcoes:",
    "  --lexer-only <arquivo>   : Realiza a analise lexica do programa.",
    "  --parse-only <arquivo>   : Realiza a analise sintatica do programa.",
    "  --interpret <arquivo>    : Interpreta o programa de entrada.",
    "  --v1-compile <arquivo>   : Compila o programa para codigo da maquina virtual V1.",
    "  --c-compile <arquivo>    : Compila o programa para um executavel usando C e GCC.",
    "  --help                   : Exibe esta mensagem de ajuda."
    ]

lexerOnly :: FilePath -> IO ()
lexerOnly file = do
  content <- readFile file
  let tokens = lexer content
  mapM_ printToken tokens

parserOnly :: FilePath -> IO ()
parserOnly file = do
  content <- readFile file
  case l2Parser content of
    Left err -> do
      putStrLn "Erro na analise sintatica:"
      putStrLn err
    Right ast -> do
      putStrLn "Analise sintatica concluida."
      putStrLn "AST:"
      putStrLn (show (ppr ast))

printToken :: Token -> IO ()
printToken (Token (l, c) lexeme) =
  putStrLn $ case lexeme of
    TDef        -> "Palavra reservada def Linha:" ++ show l ++ " Coluna:" ++ show c
    TIn         -> "Palavra reservada in Linha:" ++ show l ++ " Coluna:" ++ show c
    TEnd        -> "Palavra reservada end Linha:" ++ show l ++ " Coluna:" ++ show c
    TAssign     -> "Atribuicao := Linha:" ++ show l ++ " Coluna:" ++ show c
    TRead       -> "Palavra reservada read Linha:" ++ show l ++ " Coluna:" ++ show c
    TPrint      -> "Palavra reservada print Linha:" ++ show l ++ " Coluna:" ++ show c
    TPlus       -> "Operador + Linha:" ++ show l ++ " Coluna:" ++ show c
    TMinus      -> "Operador - Linha:" ++ show l ++ " Coluna:" ++ show c
    TTimes      -> "Operador * Linha:" ++ show l ++ " Coluna:" ++ show c
    TDiv        -> "Operador / Linha:" ++ show l ++ " Coluna:" ++ show c
    TLParen     -> "Parenteses ( Linha:" ++ show l ++ " Coluna:" ++ show c
    TRParen     -> "Parenteses ) Linha:" ++ show l ++ " Coluna:" ++ show c
    TSemicolon  -> "Ponto e virgula ; Linha:" ++ show l ++ " Coluna:" ++ show c
    TComma      -> "Virgula , Linha:" ++ show l ++ " Coluna:" ++ show c
    TNumber n   -> "Numero " ++ show n ++ " Linha:" ++ show l ++ " Coluna:" ++ show c
    TString s   -> "String \"" ++ s ++ "\" Linha:" ++ show l ++ " Coluna:" ++ show c
    TId s       -> "Identificador " ++ s ++ " Linha:" ++ show l ++ " Coluna:" ++ show c
    TEOF        -> "EOF Linha:" ++ show l ++ " Coluna:" ++ show c