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
evalInt (Const i) st = i -- [| i |] st = i
evalInt (Var v) st = get st v -- [| v |] st = st v
evalInt (Neg e) st = - (evalInt e st) -- [| -e |] st = - [| e |] st
evalInt (IBin Plus e1 e2) st = (evalInt e1 st) + (evalInt e2 st) -- [| e1 + e2 |] st = [| e1 |] st + [| e2 |] st
evalInt (IBin Minus e1 e2) st = (evalInt e1 st) - (evalInt e2 st) -- [| e1 - e2 |] st = [| e1 |] st - [| e2 |] st
evalInt (IBin Times e1 e2) st = (evalInt e1 st) * (evalInt e2 st) -- [| e1 * e2 |] st = [| e1 |] st * [| e2 |] st
evalInt (IBin Div e1 e2) st = (evalInt e1 st) `div` (evalInt e2 st) -- [| e1 / e2 |] st = [| e1 |] st / [| e2 |] st
evalInt (IBin Mod e1 e2) st = (evalInt e1 st) `mod` (evalInt e2 st) -- [| e1 % e2 |] st = [| e1 |] st % [| e2 |] st


-- | Evaluación de expresiones booleanas en bool.
evalBool :: Assert -> State -> Bool
evalBool CTrue st = True -- [| true |] st = True
evalBool CFalse st = False -- [| false |] st = False
evalBool (ABin And b1 b2) st = (evalBool b1 st) && (evalBool b2 st) -- [| b1 and b2 |] st = [| b1 |] st and [| b2 |] st
evalBool (ABin Or b1 b2) st = (evalBool b1 st) || (evalBool b2 st) -- [| b1 or b2 |] st = [| b1 |] st or [| b2 |] st
evalBool (IntAss Lt e1 e2) st = (evalInt e1 st) < (evalInt e2 st) -- [| b1 < b2 |] st = [| b1 |] st < [| b2 |] st
evalBool (IntAss Eq e1 e2) st = (evalInt e1 st) == (evalInt e2 st) -- [| b1 == b2 |] st = [| b1 |] st == [| b2 |] st
evalBool (IntAss NEq e1 e2) st = (evalInt e1 st) /= (evalInt e2 st) -- [| b1 != b2 |] st = [| b1 |] st != [| b2 |] st


-- | La evaluación de un comando es un elemento de Ω; el único
-- elemento que no tiene una representación explícita es ⊥. ¿Por qué?
-- Para definir esta función les pueden ser muy útil las funciones que
-- están debajo.
eval :: Comm -> State -> Omega
eval Skip st = Term st -- [| skip |] st = ι_term st
eval Morite st = Abort st  -- [| fail |] st = ι_abort st
eval (Assign v e) st = Term (set st v (evalInt e st)) -- [| v := e |] st = ι_term [st | v: [|e|]st]
eval (Toma e) st = Out (evalInt e st) (Term st) -- [| !e |] st = ι_out([|e|] st, ι_term st)
eval (Dame v) st = In v (\n -> (Term (set st v n))) -- [| ?v |] st = ι_in(\n in Z, ι_term [st | v: n])
eval (If b c0 c1) st = if (evalBool b st) then (eval c0 st) -- [| c0 |] st   si [| b |] st
                       else (eval c1 st)                    -- [| c1 |] st   caso contrario
eval (Seq c0 c1) st = star (eval c1) (eval c0 st) -- [| c0;c1 |] st = [| c1 |]★ ([| c0 |] st)
eval (Agarrame c0 c1) st = plus (eval c1) (eval c0 st) -- [| catchin c0 with c1 |] st = [| c1 |]+ ([| c0 |] st)
eval (Newvar v e c) st = dagger restore (eval c (set st v (evalInt e st))) -- [| newvar v:=e in c|] st = 
                                where restore st' = (set st' v (get st v)) -- (\st' in SIGMA. [st'|v: st v])† = ([| c |] [st | v: [|e|]st])
eval (While b c) st = fix fwhile st
                          where fwhile w st' | evalBool b st' = star w (eval c st') -- w★([|c|] st')   si [| b |] st'
                                             | otherwise = Term st'                 -- ι_term st'      caso contrario


-- | La función star (★) extiende funciones de Σ → Ω a funciones de Ω → Ω.
-- ¿Por qué no propagamos ⊥? ¿O sí lo estamos propagando?
-- -> Se transfiere el control a f sólo en caso de una situación no abortiva.
star :: (State -> Omega) -> Omega -> Omega
star f (Term st) = f st -- f★ (ι_term st) = f st
star f (Abort st) = Abort st -- f★ (ι_abort st) = ι_abort st
star f (Out n w) = Out n (star f w) -- f★ (ι_out (n, w)) = ι_out (n, f★ w)
star f (In v g) = In v (star f . g) -- f★ (ι_in g) = ι_in (f★ . g)
                                    -- esto es equivalente a \n -> star f (g n)


-- | la función plus (+) extiende funciones de Σ → Ω a funciones de Ω → Ω.
-- -> Se transfiere el control a f sólo en caso de excepción.
plus :: (State -> Omega) -> Omega -> Omega
plus f (Term st) = Term st -- f+ (ι_term st) = ι_term st
plus f (Abort st) = f st -- f+ (ι_abort st) = f st
plus f (Out n w) = Out n (plus f w) -- f+ (ι_out (n, w)) = ι_out (n, f+ w)
plus f (In v g) = In v (plus f . g) -- f+ (ι_in g) = ι_in (f+ . g)
                                    -- esto es equivalente a \n -> plus f (g n)


-- | La función dagger (†) extiende funciones de Σ → Σ a funciones de Ω → Ω,
-- aplicando la función al estado resultante en caso de terminación
-- normal o anormal y en los casos de input y output se comporta similar a
-- star.
-- -> Se transfiere el control a f en cualquier situación (abortiva o no).
dagger :: (State -> State) -> Omega -> Omega
dagger f (Term st) = Term (f st) -- f† (ι_term st) = ι_term (f st)
dagger f (Abort st) = Abort (f st) -- f† (ι_abort st) = ι_abort (f st)
dagger f (Out n w) = Out n (dagger f w) -- f† (ι_out (n, w)) = ι_out (n, f† w)
dagger f (In v g) = In v (dagger f . g) -- f† (ι_in g) = ι_in (f† . g)
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
