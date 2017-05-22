-- | Definición del lenguaje.
module Lang where

-- | Los nombres de variables son strings, pero quizás 
-- querramos cambiarlo luego.
type Iden = String

-- | Expresiones enteras 
data IntExpr = Const Int
             | Var Iden
             | Neg IntExpr
             | IBin OpInt IntExpr IntExpr
             
isBasic (Const _) = True
isBasic (Var _) = True
isBasic _ = False

instance Show IntExpr where
    show (Const c) = show c
    show (Var s) = s
    show (Neg e) = '-':(if isBasic e then show e else "...")
    show (IBin o e1 e2) = if isBasic e1 && isBasic e2 then (show e1) ++ (show o) ++ (show e2)
                            else "..." ++ (show o) ++ "..."

infixr 6 .+
infixr 6 .- 
infixr 5 .*
infixr 5 ./
infixr 5 .| 
-- | Smart-constructors para escribir menos.
(.+) :: IntExpr -> IntExpr -> IntExpr
(.+) = IBin Plus
(.-) :: IntExpr -> IntExpr -> IntExpr
(.-) = IBin Minus
(.*) :: IntExpr -> IntExpr -> IntExpr
(.*) = IBin Times
(./) :: IntExpr -> IntExpr -> IntExpr
(./) = IBin Div
(.|) :: IntExpr -> IntExpr -> IntExpr
(.|) = IBin Mod
v :: Iden -> IntExpr
v = Var
c :: Int -> IntExpr
c = Const

-- | Operadores binarios enteros, `Div` corresponde al cociente.
data OpInt = Plus | Minus | Times | Div | Mod
           
instance Show OpInt where
    show Plus = "+"
    show Minus = "-"
    show Times = "*"
    show Div = "/"
    show Mod = "mod" 

-- | Expresiones booleanas
data Assert = CTrue
            | CFalse
            | Not Assert
            | ABin OpBool Assert Assert
            | IntAss OpRel IntExpr IntExpr

isBasicAss CTrue = True
isBasicAss CFalse = True
isBasicAss _ = False

instance Show Assert where
    show CTrue = "true"
    show CFalse = "false"
    show (Not e) = '~':(if isBasicAss e then show e else "...")
    show (ABin o e1 e2) = if isBasicAss e1 && isBasicAss e2 then (show e1) ++ (show o) ++ (show e2)
                                        else "..." ++ (show o) ++ "..."
    show (IntAss o e1 e2) = if isBasic e1 && isBasic e2 
        then (show e1) ++ (show o) ++ (show e2)
        else "..." ++ (show o) ++ "..."

infixr 8 .&&
infixr 7 .||
-- | Smart constructors para booleanos
true,false :: Assert
true = CTrue
false = CFalse
(.&&) :: Assert -> Assert -> Assert
(.&&) = ABin And
(.||) :: Assert -> Assert -> Assert
(.||) = ABin Or
(.<) :: IntExpr -> IntExpr -> Assert
(.<) = IntAss Lt
(.=) :: IntExpr -> IntExpr -> Assert
(.=) = IntAss Eq
(.!=) :: IntExpr -> IntExpr -> Assert
(.!=) = IntAss NEq


-- | Operadores de relación 
data OpRel = Eq | NEq | Lt 

instance Show OpRel where
    show Eq = "=="
    show NEq = "!="
    show Lt = "<"          

-- | Operadores binarios booleanos 
data OpBool = And | Or

instance Show OpBool where
    show And = "/\\"
    show Or = "\\/"

data Comm = Skip 
        | Morite --corresponde a abort
        | Assign Iden IntExpr -- v := e
        | If Assert Comm Comm -- if b then c else c'
        | Seq Comm Comm -- c;c'
        | While Assert Comm -- while b do c
        | Newvar Iden IntExpr Comm -- newvar v in c
        | Dame Iden -- ?x
        | Toma IntExpr -- !x
        | Agarrame Comm Comm --catch c in c'
        
            
instance Show Comm where
    show Skip = "skip"
    show Morite = "abort"
    show (Assign v e) = v ++ ":=" ++ (show e)
    show (If b c1 c2) = "if "++ (show b) ++ "\nthen{\n" ++ (show c1) ++ "}\nelse{" ++ (show c2) ++ "}" 
    show (Seq c1 c2) = (show c1) ++ ";\n" ++ (show c2)
    show (While b c) = "while "++ (show b) ++ "do\n" ++ (show c) ++ "\nod"
    show (Newvar v e c) = "newvar " ++ v ++ " =" ++ (show e) ++ " in{\n" ++ (show c) ++ "}"
    show (Dame v) = '?':v
    show (Toma v) = '!': show v
    show (Agarrame c1 c2) = "catch {\n" ++ (show c1) ++ "}\n in{\n" ++ (show c2) ++ "}"


infix 3 .:=
infix 2 .#
-- | Smart constructors para comandos
skip :: Comm
skip = Skip
abort :: Comm
abort = Morite
(.:=) :: Iden -> IntExpr -> Comm
(.:=) = Assign
(.#) :: Comm -> Comm -> Comm
(.#) = Seq
(.?) :: Iden -> Comm
(.?) = Dame
(.!) :: IntExpr -> Comm
(.!) = Toma
catch :: Comm -> Comm -> Comm
catch = Agarrame

