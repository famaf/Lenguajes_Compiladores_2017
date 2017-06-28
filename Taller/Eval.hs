-- | Módulo para evaluar comandos del lenguaje imperativo simple.
module Eval where

import Lang

-- El módulo 'State' exporta las funciones 'get :: State -> String -> Int'
-- y 'set :: State -> String -> Int -> State'; no pueden usar la
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
evalInt (Var v) st = get st v
evalInt (Neg e) st = - (evalInt e st)
evalInt (IBin Plus e1 e2) st = (evalInt e1 st) + (evalInt e2 st)
evalInt (IBin Minus e1 e2) st = (evalInt e1 st) - (evalInt e2 st)
evalInt (IBin Times e1 e2) st = (evalInt e1 st) * (evalInt e2 st)
evalInt (IBin Div e1 e2) st = (evalInt e1 st) `div` (evalInt e2 st)
evalInt (IBin Mod e1 e2) st = (evalInt e1 st) `mod` (evalInt e2 st)

-- | Evaluación de expresiones booleanas en bool.
evalBool :: Assert -> State -> Bool
evalBool CTrue st = True
evalBool CFalse st = False
evalBool (ABin And b1 b2) st = (evalBool b1 st) && (evalBool b2 st)
evalBool (ABin Or b1 b2) st = (evalBool b1 st) || (evalBool b2 st)
evalBool (IntAss Lt e1 e2) st = (evalInt e1 st) < (evalInt e2 st)
evalBool (IntAss Eq e1 e2) st = (evalInt e1 st) == (evalInt e2 st)
evalBool (IntAss NEq e1 e2) st = (evalInt e1 st) /= (evalInt e2 st)

-- | La evaluación de un comando es un elemento de Ω; el único
-- elemento que no tiene una representación explícita es ⊥. ¿Por qué?
-- Para definir esta función les pueden ser muy útil las funciones que
-- están debajo.
eval :: Comm -> State -> Omega
eval Skip st = Term st -- ι_term st
eval Morite st = Abort st  -- ι_abort st
eval (Assign v e) st = Term (set st v (evalInt e st)) -- ι_term [st | v: [|e|]st]
eval (Toma e) st = Out (evalInt e st) (Term st) -- ι_out([|e|]st, ι_term st)
eval (Dame v) st = In v (\n -> (Term (set st v n))) -- ι_in(\n in Z, ι_term [st | v: n])
eval (If b c0 c1) st = if (evalBool b st) then (eval c0 st)
                       else (eval c1 st)
eval (Seq c0 c1) st = star (eval c1) (eval c0 st)
eval (While b c) st = fix fwhile st
                          where fwhile w st' | evalBool b st' = star w (eval c st')
                                             | otherwise = Term st'
eval (Newvar v e c) st = dagger restore (eval c (set st v (evalInt e st)))
                                where restore st' = (set st' v (get st v))


eval (Agarrame c0 c1) st = plus (eval c1) (eval c0 st)


-- | La función star (★) extiende funciones de Σ → Ω a funciones de Ω → Ω.
-- ¿Por qué no propagamos ⊥? ¿O sí lo estamos propagando?
star :: (State -> Omega) -> Omega -> Omega
star f (Term st) = f st -- Caso: f★ (ι_term st)
star f (Abort st) = Abort st -- Caso: f★ (ι_abort st)
star f (Out n w) = Out n (star f w) -- Caso: f★ (ι_out (n, w))
star f (In v g) = In v (star f . g) -- Caso: f★ (ι_in g)
                                    -- esto es equivalente a \n -> star f (g n)


-- | la función plus (+) extiende funciones de Σ → Ω a funciones de Ω → Ω.
plus :: (State -> Omega) -> Omega -> Omega
plus f (Term st) = Term st -- Caso: f+ (ι_term st)
plus f (Abort st) = f st -- Caso: f+ (ι_abort st)
plus f (Out n w) = Out n (plus f w) -- Caso: f+ (ι_out (n, w))
plus f (In v g) = In v (plus f . g) -- Caso: f+ (ι_in g)
                                    -- esto es equivalente a \n -> plus f (g n)


-- | La función dagger (†) extiende funciones de Σ → Σ a funciones de Ω → Ω,
-- aplicando la función al estado resultante en caso de terminación
-- normal o anormal y en los casos de input y output se comporta similar a
-- star.
dagger :: (State -> State) -> Omega -> Omega
dagger f (Term st) = Term (f st) -- Caso: f† (ι_term st)
dagger f (Abort st) = Abort (f st) -- Caso: f† (ι_abort st)
dagger f (Out n w) = Out n (dagger f w) -- Caso: f† (ι_out (n, w))
dagger f (In v g) = In v (dagger f . g) -- Caso: f† (ι_in g)
                                        -- esto es equivalente a \n -> dagger f (g n)


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
