module L.L2.Interpreter.Interp where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import L.L2.Frontend.Syntax
import Utils.Value
import Utils.Var


data InterpEnv = InterpEnv {
  sigma :: Map Var Value,
  phi   :: Map Var Value
}

type InterpM a = StateT InterpEnv IO a

interpL2 :: L2 -> IO ()
interpL2 (L2 stmts) = evalStateT (interpBlock stmts) initialEnv
  where
    initialEnv = InterpEnv Map.empty Map.empty

interpBlock :: [S2] -> InterpM ()
interpBlock = mapM_ interpS

interpS :: S2 -> InterpM ()
interpS (LAssign var expr) = do
  val <- evalE expr
  env <- get
  put (env { sigma = Map.insert var val (sigma env) })

interpS (LPrint expr) = do
  val <- evalE expr
  liftIO $ print val

interpS (LRead prompt var) = do
  liftIO $ putStr prompt
  input <- liftIO getLine
  let val = VInt (read input)
  env <- get
  put (env { sigma = Map.insert var val (sigma env) })

interpS (Def var expr stmts) = do
  val <- evalE expr
  
  originalPhi <- gets phi

  modify (\env -> env { phi = Map.insert var val (phi env) })

  interpBlock stmts

  modify (\env -> env { phi = originalPhi })

evalE :: E2 -> InterpM Value
evalE (LVal val) = return val

evalE (LVar var) = do
  env <- get
  case Map.lookup var (phi env) of
    Just val -> return val 
    Nothing  ->
      case Map.lookup var (sigma env) of
        Just val -> return val 
        Nothing  -> error $ "Variável não definida: " ++ show var

evalE (LAdd e1 e2) = evalBinOp (+) e1 e2
evalE (LMinus e1 e2) = evalBinOp (-) e1 e2
evalE (LMul e1 e2) = evalBinOp (*) e1 e2
evalE (LDiv e1 e2) = evalBinOp div e1 e2

-- Função auxiliar para operadores binários.
evalBinOp :: (Int -> Int -> Int) -> E2 -> E2 -> InterpM Value
evalBinOp op e1 e2 = do
  v1 <- evalE e1
  v2 <- evalE e2
  case (v1, v2) of
    (VInt i1, VInt i2) -> return $ VInt (op i1 i2)
    _ -> error "Operação aritmética em valores não inteiros."