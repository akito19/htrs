module HTRS.Type where

import Data.List
import HTRS.Term
import HTRS.SCC

data Type = TVar String
          | TCon String
          | TApp Type Type
          | TArr Type Type deriving Eq

type Env       = [(String, Type)]
type TypeSubst = [(String, Type)]

instance Show Type where
    show = showI1

showI1 (TArr t1 t2) = showI2 t1 ++ " -> " ++ showI1 t2
showI1 t            = showI2 t

showI2 (TApp t1 t2) = showI2 t1 ++ " " ++ showI3 t2
showI2 x            = showI3 x

showI3 (TVar x)     = x
showI3 (TCon x)     = x
showI3 t            = "(" ++ showI1 t ++ ")"

substitute :: Term -> Substitution -> Term
substitute term        []    = term
substitute t@(Var x)   sigma
  | Just u <- lookup x sigma = u
  | otherwise                = t
substitute t@(Con _)   sigma = t
substitute (App t1 t2) sigma = App (substitute t1 sigma) (substitute t2 sigma)

mgu :: [(Type, Type)] -> Maybe Env
mgu mu = mgu' [] mu

mgu' :: Env -> [(Type, Type)] -> Maybe Env
mgu' sigma [] = Just sigma
mgu' sigma ((TVar x, TVar f) : es)
  | x == f = mgu' sigma es
mgu' sigma ((x@(TVar f), y) : es)
  | checkOccurrence x y = Nothing
  | otherwise           = mgu' (compose sigma [(f, y)]) [ (substituteType t [(f, y)], substituteType u [(f, y)]) | (t, u) <- es]
mgu' sigma ((TCon x, TCon y) : es)
  | x == y = mgu' sigma es
mgu' sigma (((TApp t1 t2), (TApp u1 u2)) : es) = mgu' sigma ((t1, u1) : (t2, u2) : es)
mgu' sigma (((TArr t1 t2), (TArr u1 u2)) : es) = mgu' sigma ((t1, u1) : (t2, u2) : es)
mgu' sigma ((x, y@(TVar f)) : es)
  | checkOccurrence y x = Nothing
  | otherwise           = mgu' (compose sigma [(f, x)]) [ (substituteType t [(f, x)], substituteType u [(f, x)]) | (t, u) <- es ]
mgu' sigma mu = Nothing

checkOccurrence :: Type -> Type -> Bool
checkOccurrence x (TApp u1 u2)
  | x == u2   = True
  | x == u1   = True
  | otherwise = checkOccurrence x u2 || checkOccurrence x u1
checkOccurrence _ _ = False

domain :: Env -> [String]
domain sigma = [ x | (x, t) <- sigma, TVar x /= t ]

compose :: Env -> Env -> Env
compose sigma tau = [ (x, substituteType (substituteType (TVar x) sigma) tau) | x <- nub (domain sigma ++ domain tau)]

renameType :: Type -> Int -> Type
renameType (TVar f)     i = TVar (composeVar ("d_" ++ f) i)
renameType c@(TCon _)   i = c
renameType (TApp t1 t2) i = TApp (renameType t1 i) (renameType t2 i)
renameType (TArr t1 t2) i = TArr (renameType t1 i) (renameType t2 i)

substituteType :: Type -> TypeSubst -> Type
substituteType t           [] = t
substituteType tv@(TVar x) ((s, t) : ts)
  | x == s    = t
  | otherwise = substituteType tv ts
substituteType tc@(TCon f) ((s, t) : ts)
  | f == s    = t
  | otherwise = substituteType tc ts
substituteType (TApp t1 t2) subst = TApp (substituteType t1 subst) (substituteType t2 subst)
substituteType (TArr t1 t2) subst = TArr (substituteType t1 subst) (substituteType t2 subst)

inferTerm :: Env -> Term -> Int -> ([(Type, Type)], Int)
inferTerm env (Var x) i    = ([(TVar (composeVar "a_" i), TVar ("d_" ++ x))], i + 1)
inferTerm env (Con f) i
  | Just t <- lookup f env = ([(TVar (composeVar "a_" i), renameType t i)], i + 1)
  | otherwise              = ([(TVar (composeVar "a_" i), TVar ("d_" ++ f))], i + 1)
inferTerm env (App t u) i  = ((es0 : (es1 ++ es2)), k)
    where
        es0 = (TVar (composeVar "a_" (i + 1)), TArr (TVar (composeVar "a_" j)) (TVar (composeVar "a_" i)))
        (es1, j) = inferTerm env t (i + 1)
        (es2, k) = inferTerm env u j

inferRule :: Env -> Rule -> Int -> ([(Type, Type)], Int)
inferRule env (l, r) i = ((es0 : (es1 ++ es2)), n)
    where
        (es1, m) = inferTerm env l i
        (es2, n) = inferTerm env r m
        es0 = (TVar (composeVar "a_" i), (TVar (composeVar "a_" m)))

inferTRS :: Env -> TRS -> Int -> ([(Type, Type)], Int)
inferTRS env []       i = ([], i)
inferTRS env (t : []) i = inferRule env t i
inferTRS env (t : ts) i = ((es1 ++ es2), n)
    where
        (es1, m) = inferRule env t  i
        (es2, n) = inferTRS  env ts m

composeVar :: String -> Int -> String
composeVar s i = s ++ show i

sigTRS :: Env -> TRS -> [[String]] -> Env
sigTRS env trs []         = env
sigTRS env trs (s : sigs) =
    case mgu $ fst $ inferTRS env trs' 1 of
      Just ienv -> sigTRS (nub $ env ++ extract ienv) trs sigs
      -- TODO: Revise error handling just when infering failed.
      Nothing   -> error "Failed to calculate unification."
    where
        trs' = [ (l, r) | (l, r) <- trs, x <- s, x == headSymbol l ]
        extract es = nub [ (f, t) | (l, _) <- trs', let f = headSymbol l, let Just t = lookup ("d_" ++ f) es ]

renameTRS :: Int -> TRS -> TRS
renameTRS i []       = []
renameTRS i (t : ts) = (rule : rules)
    where
        (m, rule) = renameRule i t
        rules = renameTRS m ts

renameRule :: Int -> Rule -> (Int, Rule)
renameRule i rule@(l, _) = (i + length xs, substituteRule rule sigma)
    where
        xs    = variables l
        sigma = [ (x, Var (composeVar "x_" n)) | (n, x) <- zip [i..] xs ]

substituteRule :: Rule -> Substitution -> Rule
substituteRule (l, r) sigma = (substitute l sigma, substitute r sigma)

variables :: Term -> [String]
variables (Var x)     = [x]
variables (Con x)     = []
variables (App t1 t2) = variables t1 ++ variables t2

dependency :: TRS -> [(String, String)]
dependency trs = nub [ (fs, headSymbol l) | (l, r) <- trs, fs <- funcSymbols r, elem fs hs ]
    where
        hs = headSymbols trs

funcSymbols :: Term -> [String]
funcSymbols (Var f)     = []
funcSymbols (Con x)     = [x]
funcSymbols (App t1 t2) = funcSymbols t1 ++ funcSymbols t2

headSymbol :: Term -> String
headSymbol (Var f)     = f
headSymbol (Con x)     = x
headSymbol (App t1 t2) = headSymbol t1

headSymbols :: TRS -> [String]
headSymbols trs = nub [ headSymbol l | (l, r) <- trs ]

typeVariables :: Type -> [String]
typeVariables (TVar f)     = [f]
typeVariables (TCon _)     = []
typeVariables (TApp t1 t2) = typeVariables t1 ++ typeVariables t2
typeVariables (TArr t1 t2) = typeVariables t1 ++ typeVariables t2

beautify :: Env -> Env
beautify env = [ (l, beautify' r vars) | (l, r) <- env, let vars = zip (typeVariables r) ['a'..'z'] ]

beautify' :: Type -> [(String, Char)] -> Type
beautify' (TApp t1 t2) vars = TApp (beautify' t1 vars) (beautify' t2 vars)
beautify' (TArr t1 t2) vars = TArr (beautify' t1 vars) (beautify' t2 vars)
beautify' c@(TCon _)   vars = c
beautify' v@(TVar f)   vars
  | Just x <- lookup f vars = (TVar [x])
  | otherwise               = v

signature :: Env -> TRS -> Env
signature env trs = beautify $ sigTRS env trs' sig
    where
        trs' = renameTRS 1 trs
        sig  = sccs (headSymbols trs') (dependency trs')
