{-
Sea:
    * τ tipo (de los de Logica)
    * C = {c, d}
    * F = {f, g}
    * a(f) = 2 y a(g) = 3

Talque:
    τ = (C, F, Ø, a)
-}


data Termino = Var Int | C | D | F Termino Termino | G Termino Termino Termino

concr :: Termino -> String
concr (Var n) = 'X' : show n
concr C = "c"
concr D = "d"
concr (F t1 t2) = concat ["f(", concr t1, ",", concr t2, ")"]
concr (G t1 t2 t3) = concat ["g(", concr t1, ",", concr t2, ",", concr t3, ")"]

{-
Correr la funcion "concr" con los siguientes ejemplos:

t1 = Var 2
t2 = G (Var 0) (F C D) C
t3 = F t2 t1
-}
