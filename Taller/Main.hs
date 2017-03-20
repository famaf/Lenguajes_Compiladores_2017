module Main where

import Control.Monad
import System.Environment
import System.Exit

import Eval
import Lang (Comm)
import State (initState)


-- | La interpretación de la descripción de comportamientos de los
-- programas es interpretada como una computación en la mónada de IO.
run :: Omega -> IO ()
run (Term st) = return ()
run (Abort st) = putStrLn "El programa terminó con una falla."
run (Out n w) = putStrLn (show n) >> run w
run (In v g) = putStr (v ++ ": ") >> getLine >>= run . g . read

exec comm = run $ eval comm initState

-- Pueden probarlo con
{-
$ ghci Main Examples
*Main> import Examples
*Main Examples> exec commex

-}
