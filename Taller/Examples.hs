module Examples where

import Lang

-- Ejemplos
-- x + 1
equismasuno :: IntExpr
equismasuno = v "x" .+ c 1

-- x + 1 != x + 1
assex = equismasuno .!= equismasuno


-- ejemplo de comando
commex = Newvar "x" {- := -} (c 3 .+ c 1) {- in -}
           (If (v "x" .< equismasuno)
{- then -} ("x" .:= equismasuno .# ("x" .:= equismasuno))
{- else -} ("y" .:= equismasuno))

commex_3 = Seq (Dame "x") (Toma (Var "x"))

commex_2 = While CTrue Skip
