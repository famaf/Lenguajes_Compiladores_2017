module State (State,get,set,initState) where

newtype State = State [(String,Int)]


get :: State -> String -> Int
get (State s) v = maybe 0 id $ lookup v s

set :: State -> String -> Int -> State
set (State s) v i = State $ update s v i
  where update [] v i = [(v,i)]
        update ((v',j):vs) v i | v == v' = (v,i):vs
                               | v /= v' = (v',j):update vs v i

instance Show State where
    show (State s) = show (take 4 s) ++ "…"

initState :: State
initState = State []
