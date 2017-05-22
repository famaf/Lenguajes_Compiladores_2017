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

commex1 =   Newvar "x" (Const (-3  )) 
                (While (IntAss Lt (Var "x") (Const 0))
                    (Seq (Assign "x" (IBin Plus (Var "x") (Const (1)))) (Toma (Var "x"))))

commex2 = While CTrue Skip            