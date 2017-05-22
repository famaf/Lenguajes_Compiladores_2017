-- | Módulo para evaluar comandos del lenguaje imperativo simple.
module Eval where

import Lang

-- El módulo 'State' exporta las funciones 'get :: State -> String ->
-- Int' y 'set :: State -> String -> Int -> State'; no pueden usar la
-- representación de 'State'.
import State

-- | La denotación de nuestros comandos será un tipo de datos que
-- describa los posibles comportamientos que hemos analizado en el
-- teórico. La variable del input la usamos para poder decir para
-- que variable se está pidiendo input.
data Omega = Term State
           | Abort State
           | Out Int Omega
           | In Iden (Int -> Omega)

-- Notemos que los elementos de Ω no tienen ninguna relación con la
-- entrada o salida; lo que haremos en cambio será luego “interpretar”
-- esas descripciones de comportamientos como computaciones en la 
-- mónada de IO.

-- | La evaluación de expresiones enteras serán enteros.
evalInt :: IntExpr -> State -> Int
evalInt (Const i) st = i
evalInt (Var s) st = get st s
evalInt (Neg ie) st = - (evalInt ie st)
evalInt (IBin Plus ie1 ie2) st = (evalInt ie1 st) + (evalInt ie2 st)
evalInt (IBin Minus ie1 ie2) st = (evalInt ie1 st) - (evalInt ie2 st)
evalInt (IBin Times ie1 ie2) st = (evalInt ie1 st) * (evalInt ie2 st)
evalInt (IBin Div ie1 ie2) st = (evalInt ie1 st) `div` (evalInt ie2 st)
evalInt (IBin Mod ie1 ie2)  st = (evalInt ie1 st) `mod` (evalInt ie2 st)

-- | Evaluación de expresiones booleanas en bool.
evalBool :: Assert -> State -> Bool
evalBool CTrue st = True
evalBool CFalse st = False
evalBool (ABin And as1 as2) st = (evalBool as1 st) && (evalBool as2 st)
evalBool (ABin Or as1 as2) st = (evalBool as1 st) || (evalBool as2 st)
evalBool (IntAss Lt ie1 ie2) st = (evalInt ie1 st) < (evalInt ie2 st)
evalBool (IntAss Eq ie1 ie2) st = (evalInt ie1 st) == (evalInt ie2 st)
evalBool (IntAss NEq ie1 ie2) st = (evalInt ie1 st) /= (evalInt ie2 st)

-- | La evaluación de un comando es un elemento de Ω; el único
-- elemento que no tiene una representación explícita es ⊥. ¿Por qué?
-- Para definir esta función les pueden ser muy útil las funciones que
-- están debajo.

eval :: Comm -> State -> Omega
eval (While b c) st = fix fwhile st
    where fwhile w st' | evalBool b st' = star w (eval c st')
                       | otherwise = (Term st')
eval (Skip) st = (Term st)
eval (Morite) st = (Abort st)
eval (Assign var ie) st = (Term (set st var (evalInt(ie) st)))
eval (If assert c0 c1) st = if (evalBool assert) st then eval c0 st else eval c1 st
eval (Seq c0 c1) st = star (eval c1) (eval c0 st)
eval (Newvar var ie c1) st = dagger restaurar (eval (c1) (set st var (evalInt ie st)))
                                where restaurar st' = (set st' var (get st var))
eval (Agarrame c0 c1) st = plus (eval c0) (eval c1 st)
eval (Toma ie) st = (Out (evalInt ie st ) (Term st))
eval (Dame var) st = (In var (\n -> (Term (set st var n))))


-- | La función star (★) extiende funciones de Σ → Ω a funciones de Ω → Ω. 
-- ¿Por qué no propagamos ⊥? ¿O sí lo estamos propagando?
star :: (State -> Omega) -> Omega -> Omega
star f (Term st) = f st
star f (Abort st) = Abort st
star f (Out n w) = Out n (star f w)
star f (In v g) = In v (star f . g) -- esto es equivalente a \n -> star f (g n)

-- | La función dagger (†) extiende funciones de Σ → Σ a funciones de
-- Ω → Ω, aplicando la función al estado resultante en caso de terminación
-- normal o anormal y en los casos de input y output se comporta similar a
-- star.
dagger :: (State -> State) -> Omega -> Omega
dagger f (Term st) = Term (f st)
dagger f (Abort st) = Abort (f st)
dagger f (Out n w) = Out n (dagger f w)
dagger f (In v g) = In v (dagger f . g)

-- | La funcion (+)

plus :: (State -> Omega) -> Omega -> Omega
plus f (Term st) = (Term st)
plus f (Abort st) = (f st)
plus f (Out n w) = Out n (plus f w)
plus f (In v g) = In v (plus f . g)

-- | En Haskell podemos definir una función que resembla el operador Y del
-- menor punto fijo.
fix :: (d -> d) -> d
fix f = f (fix f)

-- | La siguiente función de alto-orden tiene como menor punto fijo el
--  factorial, para los positivos...
fact :: (Int -> Int) -> (Int -> Int)
fact f 0 = 1
fact f n = n * fact f (n-1)

-- | y de hecho podemos definirlo como tal.
factorial :: Int -> Int
factorial = fix fact

{- 
Para convencerse de que funciona podemos pensar cómo se evalúa la
expresión @factorial 4@:

factorial 4 = (fix fact) 4
→ 
fact (fix fact) 4
→
4 * fact (fix fact) 3
→
4 * (3 * fact (fix fact) 2)
→ 
4 * (3 * (2 * fact (fix fact) 1))
→
4 * (3 * (2 * (1 * fact (fix fact) 0)))
→
4 * (3 * (2 * (1 * 1)))
→→
24
-}
