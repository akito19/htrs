module HTRS.Term where

data Term = Var String
          | Con String
          | App Term Term deriving Eq

data MarkedTerm = MApp MarkedTerm MarkedTerm
                | MCon String
                | NF Term

data Context = Hole
             | CApp1 Context Term
             | CApp2 MarkedTerm Context

type Rule         = (Term, Term) 
type TRS          = [Rule]
type Substitution = [(String, Term)]

instance Show Term where
    show = showT1

showT1 t@(App t1 t2)
  | not (isNumber t) && not (isList t) = showT1 t1 ++ " " ++ showT2 t2
showT1 x = showT2 x

showT2 (Var x) = x
showT2 (Con x) = x
showT2 t
  | isNumber t = show $ convertNum t
  | isList t   = show $ convertList t
showT2 x       = "(" ++ showT1 x ++ ")"

instance Show MarkedTerm where
    show = showM1

showM1 t@(MApp t1 t2) = showM1 t1 ++ " " ++ showM2 t2
showM1 x              = showM2 x

showM2 (NF (App x y)) = "(" ++ showT1 (App x y) ++ ")"
showM2 (NF (Var x))   = x
showM2 (NF (Con x))   = x
showM2 (MCon x)       = x
showM2 x              = "(" ++ showM1 x ++ ")"

instance Show Context where
    show (Hole) = "{}"
    show (CApp1 c t) = "CApp1" ++ " (" ++ show c ++ ")" ++ " (" ++ show t ++ ")"
    show (CApp2 t c) = "CApp2" ++ " (" ++ show t ++ ")" ++ " (" ++ show c ++ ")"

isNumber :: Term -> Bool
isNumber (Con "0")         = True
isNumber (App (Con "s") t) = isNumber t
isNumber _                 = False

isList :: Term -> Bool
isList (Con "nil")                  = True
isList (App (App (Con "cons") _) t) = isList t
isList _                            = False

convertNum :: Term -> Int
convertNum (Con "0")         = 0
convertNum (App (Con "s") t) = 1 + convertNum t

convertList :: Term -> [Term]
convertList (Con "nil")                   = []
convertList (App (App (Con "cons") t) ts) = t : convertList ts

