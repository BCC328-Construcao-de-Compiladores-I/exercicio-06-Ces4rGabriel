module L.L2.Backend.V1Codegen where
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import L.L2.Frontend.Syntax
import Utils.Value
import Utils.Var

data V1_Ins
  = PUSH Int
  | PUSHS String
  | ADD | SUB | MUL | DIV
  | LOAD Var
  | STORE Var
  | READ Var String
  | PRINT
  | POP Int 
  deriving (Eq, Ord)

instance Show V1_Ins where
  show (PUSH n)    = "push " ++ show n
  show (PUSHS s)   = "pushs \"" ++ s ++ "\""
  show ADD         = "add"
  show SUB         = "sub"
  show MUL         = "mul"
  show DIV         = "div"
  show (LOAD v)    = "load " ++ show v
  show (STORE v)   = "store " ++ show v
  show (READ v s)  = "read " ++ show v ++ ", \"" ++ s ++ "\""
  show PRINT       = "print"
  show (POP n)     = "pop " ++ show n

type CgEnv = Map Var Int
type CgM a = State CgEnv a

v1gen :: L2 -> [V1_Ins]
v1gen (L2 stmts) = evalState (cgStmts stmts) Map.empty

cgStmts :: [S2] -> CgM [V1_Ins]
cgStmts stmts = concat <$> mapM cgS stmts

cgS :: S2 -> CgM [V1_Ins]
cgS (LAssign v e) = do
  insE <- cgE e
  return $ insE ++ [STORE v]
cgS (LPrint e) = do
  insE <- cgE e
  return $ insE ++ [PRINT]
cgS (LRead prompt v) = return [READ v prompt]
cgS (Def v e stmts) = do
  insE <- cgE e

  originalEnv <- get


  let newEnv = Map.insert v 0 (Map.map (+1) originalEnv)
  put newEnv

  insStmts <- cgStmts stmts

  put originalEnv

  return $ insE ++ insStmts ++ [POP 1]

cgE :: E2 -> CgM [V1_Ins]
cgE (LVal (VInt n)) = return [PUSH n]
cgE (LVal (VStr s)) = return [PUSHS s]
cgE (LAdd e1 e2) = cgBinOp e1 e2 ADD
cgE (LMinus e1 e2) = cgBinOp e1 e2 SUB
cgE (LMul e1 e2) = cgBinOp e1 e2 MUL
cgE (LDiv e1 e2) = cgBinOp e1 e2 DIV
cgE (LVar v) = do
  env <- get
  case Map.lookup v env of
   
    Just _offset -> return [LOAD v]
    Nothing    -> return [LOAD v]

cgBinOp :: E2 -> E2 -> V1_Ins -> CgM [V1_Ins]
cgBinOp e1 e2 op = do
  ins1 <- cgE e1
  ins2 <- cgE e2
  return $ ins1 ++ ins2 ++ [op]