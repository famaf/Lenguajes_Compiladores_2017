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
evalInt e st = undefined

-- | Evaluación de expresiones booleanas en bool.
evalBool :: Assert -> State -> Bool
evalBool e st = undefined

-- | La evaluación de un comando es un elemento de Ω; el único
-- elemento que no tiene una representación explícita es ⊥. ¿Por qué?
-- Para definir esta función les pueden ser muy útil las funciones que
-- están debajo.
eval :: Comm -> State -> Omega
eval (While b c) st = fix fwhile st
    where fwhile w st' | evalBool b st' = undefined
                       | otherwise = undefined
eval c st = undefined


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
dagger f w = undefined

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
