module L.L2.Frontend.Syntax where
import Utils.Value
import Utils.Var
import Utils.Pretty

data L2 = L2 [S2]
  deriving (Eq, Ord, Show)

data S2
  = Def Var E2 [S2]
  | LRead String Var
  | LPrint E2
  | LAssign Var E2
  deriving (Eq, Ord, Show)

data E2
  = LVal Value
  | LVar Var
  | LAdd E2 E2
  | LMinus E2 E2
  | LMul E2 E2
  | LDiv E2 E2
  deriving (Eq, Ord, Show)

instance Pretty L2 where
  ppr (L2 ss) = vcat (punctuate semi (map ppr ss))

instance Pretty S2 where
  ppr (LRead s v) = hsep [ text "read(", doubleQuotes (text s), comma, ppr v, text ")" ]
  ppr (LPrint e) = hsep [ text "print(", ppr e, text ")" ]
  ppr (LAssign v e) = hsep [ ppr v, text ":=", ppr e ]
  ppr (Def v e ss) = vcat [ hsep [text "def", ppr v, text ":=", ppr e, text "in"], nest 2 (vcat (punctuate semi (map ppr ss))), text "end" ]

instance Pretty E2 where
  ppr = pprAdd

pprAdd :: E2 -> Doc
pprAdd (LAdd e1 e2) = hsep [pprAdd e1, text "+", pprMul e2]
pprAdd (LMinus e1 e2) = hsep [pprAdd e1, text "-", pprMul e2]
pprAdd other = pprMul other

pprMul :: E2 -> Doc
pprMul (LMul e1 e2) = hsep [pprMul e1, text "*", pprFact e2]
pprMul (LDiv e1 e2) = hsep [pprMul e1, text "/", pprFact e2]
pprMul other = pprFact other

pprFact :: E2 -> Doc
pprFact (LVal v) = ppr v
pprFact (LVar v) = ppr v
pprFact other    = parens (ppr other)