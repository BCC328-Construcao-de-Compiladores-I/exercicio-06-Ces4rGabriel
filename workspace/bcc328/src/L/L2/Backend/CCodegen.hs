module L.L2.Backend.CCodegen where

import Data.List (nub)
import L.L2.Frontend.Syntax
import Utils.Value
import Utils.Var

-- Função principal para gerar código C a partir de um programa L2.
cgen :: L2 -> String
cgen l2@(L2 stmts) =
  unlines [ "#include <stdio.h>"
          , "#include <string.h>"
          , ""
          , "// Declaracao de variaveis mutaveis (globais)"
          , declareMutableVars l2
          , ""
          , "int main() {"
          , codeForStmts stmts
          , "  return 0;"
          , "}"
          ]

findMutableVars :: L2 -> [Var]
findMutableVars (L2 stmts) = nub $ go stmts
  where
    go [] = []
    go (LAssign var _ : xs) = var : go xs
    go (Def _ _ block : xs) = go block ++ go xs
    go (_ : xs) = go xs

declareMutableVars :: L2 -> String
declareMutableVars l2 =
  let vars = findMutableVars l2
  in unlines $ map (\v -> "int " ++ show v ++ " = 0;") vars

codeForStmts :: [S2] -> String
codeForStmts stmts = unlines $ map (indent . codeForS) stmts
  where indent s = "  " ++ s

codeForS :: S2 -> String
codeForS (LAssign var expr) = show var ++ " = " ++ codeForE expr ++ ";"
codeForS (LPrint expr) = "printf(\"%d\\n\", " ++ codeForE expr ++ ");"
codeForS (LRead prompt var) =
  "printf(\"" ++ prompt ++ "\"); scanf(\"%d\", &" ++ show var ++ ");"
codeForS (Def var expr stmts) =
  unlines [ "{"
          , "  int " ++ show var ++ " = " ++ codeForE expr ++ ";"
          , codeForStmts stmts
          , "}"
          ]

codeForE :: E2 -> String
codeForE (LVal (VInt n)) = show n
codeForE (LVal (VStr s)) = "\"" ++ s ++ "\""
codeForE (LVar var) = show var
codeForE (LAdd e1 e2) = "(" ++ codeForE e1 ++ " + " ++ codeForE e2 ++ ")"
codeForE (LMinus e1 e2) = "(" ++ codeForE e1 ++ " - " ++ codeForE e2 ++ ")"
codeForE (LMul e1 e2) = "(" ++ codeForE e1 ++ " * " ++ codeForE e2 ++ ")"
codeForE (LDiv e1 e2) = "(" ++ codeForE e1 ++ " / " ++ codeForE e2 ++ ")"