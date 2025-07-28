module L.L2.Frontend.TypeCheck where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.List ((\\))

import L.L2.Frontend.Syntax
import Utils.Var

type TcM a = ExceptT String (WriterT [String] (StateT TcEnv Identity)) a

data TcEnv
  = TcEnv {
      context :: [Var] 
    }

initTcEnv :: TcEnv
initTcEnv = TcEnv []

insertVar :: Var -> TcM ()
insertVar v = modify (\ env -> env{context = v : context env})

removeVar :: Var -> TcM ()
removeVar v = modify (\ env -> env {context = (context env) \\ [v]})

isImmutable :: Var -> TcM Bool
isImmutable v = gets (elem v . context)

runTcM :: TcEnv -> TcM a -> (((Either String a), [String]), TcEnv)
runTcM env m
  = runIdentity (runStateT (runWriterT (runExceptT m)) env)

typeCheck :: L2 -> Either String L2
typeCheck l2 =
  let ((res, _log), _env) = runTcM initTcEnv (tcL2 l2)
  in case res of
    Left err -> Left err
    Right p  -> Right p

tcL2 :: L2 -> TcM L2
tcL2 (L2 stmts) = L2 <$> mapM tcS stmts

tcS :: S2 -> TcM S2
tcS stmt@(LAssign var expr) = do
  immutable <- isImmutable var
  when immutable $
    throwError $ "Type Error: Variavel imutavel '" ++ show var ++ "' nao pode ser reatribuida."
  e' <- tcE expr
  return (LAssign var e')

tcS (LPrint expr) = LPrint <$> tcE expr

tcS (LRead prompt var) = return (LRead prompt var)

tcS (Def var expr stmts) = do
  e' <- tcE expr
  insertVar var
  stmts' <- mapM tcS stmts
  removeVar var
  return (Def var e' stmts')

tcE :: E2 -> TcM E2
tcE (LAdd e1 e2) = LAdd <$> tcE e1 <*> tcE e2
tcE (LMinus e1 e2) = LMinus <$> tcE e1 <*> tcE e2
tcE (LMul e1 e2) = LMul <$> tcE e1 <*> tcE e2
tcE (LDiv e1 e2) = LDiv <$> tcE e1 <*> tcE e2
tcE e@(LVar _) = return e 
tcE e@(LVal _) = return e 