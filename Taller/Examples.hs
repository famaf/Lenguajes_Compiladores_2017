module Examples where

import Lang

-- Ejemplos
-- x + 1
equismasuno :: IntExpr
equismasuno = v "x" .+ c 1

-- x + 1 != x + 1
assex = equismasuno .!= equismasuno


-- ejemplo de comando
commex_1 = Newvar "x" {- := -} (c 3 .+ c 1) {- in -}
           (If (v "x" .< equismasuno)
{- then -} ("x" .:= equismasuno .# ("x" .:= equismasuno))
{- else -} ("y" .:= equismasuno))


-- ?x; !x
commex_2 = Seq (Dame "x") (Toma (Var "x"))


-- newvar x := -3 in while x < 0 do
--                       (x := x + 1); (!x)
commex_3 = Newvar "x" (Const (-3))
               (While (IntAss Lt (Var "x") (Const 0))
                   (Seq (Assign "x" (IBin Plus (Var "x") (Const (1))))
                        (Toma (Var "x"))
                   )
               )


-- Ejercicio 5)a) del Practico 4 (Modificado para que imprima la variables x)
-- ===================================================
-- while x < 2 do
--     if x < 0 then x := 0
--     else x := x + 1
-- ===================================================
-- Para x < 0 tiene que imprimir: 0, 1, 2
-- Para x = 0 tiene que imprimir: 1, 2
-- Para x = 1 tiene que imprimir: 2
-- Para x > 1 no imprime nada
-- ===================================================
commex_4 = Newvar "x" (Const (0))
               (While (IntAss Lt (Var "x") (Const 2))
                   (If (IntAss Lt (Var "x") (Const 0))
                       (Seq (Assign "x" (Const 0)) (Toma (Var "x")))
                       (Seq (Assign "x" (IBin Plus (Var "x") (Const 1))) (Toma (Var "x")))
                   )
               )


-- Ejercicio 3)1) del Practico 5 (Modificado para que imprima las variables x, y)
-- ===================================================
-- st x = 2  , st y = 1
-- (y := x + y) ; (if 0 < y then x := x - 1
--                 else skip)
-- ===================================================
-- Tiene que imprimir: 1 (corresponde a 'x')
--                     3 (corresponde a 'y')
commex_5 = Newvar "x" (Const 2)
               (Newvar "y" (Const 1)
                   (Seq (Assign "y" (IBin Plus (Var "x") (Var "y")))
                        (If (IntAss Lt (Const 0) (Var "y"))
                            (Seq (Assign "x" (IBin Minus (Var "x") (Const 1)))
                                 (Seq (Toma (Var "x"))
                                      (Toma (Var "y"))
                                 )
                            )
                            (Skip)
                        )
                   )
                )


-- Ejercicio 3)2) del Practico 5 (Modificado para que imprima las variables x, y)
-- ===================================================
-- st x = 2  , st y = 1
-- while 0 < x do
--     (y := x + y) ; (if 0 < y then x := x - 1
--                     else skip)
-- ===================================================
-- Tiene que imprimir: 1 (corresponde a 'x')
--                     3 (corresponde a 'y')
--                     0 (corresponde a 'x')
--                     4 (corresponde a 'y')
commex_6 = Newvar "x" (Const 2)
               (Newvar "y" (Const 1)
                   (While (IntAss Lt (Const 0) (Var "x"))
                       (Seq (Assign "y" (IBin Plus (Var "x") (Var "y")))
                            (If (IntAss Lt (Const 0) (Var "y"))
                                (Seq (Assign "x" (IBin Minus (Var "x") (Const 1)))
                                     (Seq (Toma (Var "x"))
                                          (Toma (Var "y"))
                                     )
                                )
                                (Skip)
                            )
                       )
                   )
                )


-- while true do skip
commex_7 = While CTrue Skip
