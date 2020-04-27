module HTRS.Rewrite (nf) where

import Data.List
import HTRS.Term

type Zipper       = (Context, MarkedTerm)

substitute :: Term -> Substitution -> MarkedTerm
substitute t@(Var x) sigma
  | Just u    <- lookup x sigma = NF u
  | otherwise                   = NF t
substitute (Con x)     sigma = MCon x
substitute (App t1 t2) sigma = MApp (substitute t1 sigma) (substitute t2 sigma)

match :: Term -> Term -> Maybe Substitution
match s t = match' [] [(s, t)]

match' :: Substitution -> [(Term, Term)] -> Maybe Substitution
match' sigma [] = Just sigma
match' sigma ((Var x, u) : ps)
  | Nothing <- lookup x sigma = match' ((x, u) : sigma) ps
  | Just t  <- lookup x sigma = if t == u then match' sigma ps else Nothing
match' sigma ((Con x, Con u) : ps)
  | x == u = match' sigma ps
match' sigma ((App t1 t2, App u1 u2) : ps) = match' sigma ((t1, u1) : (t2, u2) : ps)
match' _ _ = Nothing

reductsAtRoot :: TRS -> Term -> [MarkedTerm]
reductsAtRoot trs t = [ substitute r sigma | (l, r) <- trs, Just sigma <- [match l t] ]

rewriteAtRoot :: TRS -> Term -> Maybe MarkedTerm
rewriteAtRoot trs term =
    case reductsAtRoot trs term of
      []    -> Nothing
      r : _ -> Just r

rewrite :: TRS -> Zipper -> Zipper
rewrite trs (c, MCon x)
  | Just t    <- rewriteAtRoot trs (Con x)   = (c, t)
  | otherwise                                = rewrite trs (c, NF (Con x))
rewrite trs (c, MApp (NF x) (NF y))
  | Just t    <- rewriteAtRoot trs (App x y) = (c, t)
  | otherwise                                = rewrite trs (c, NF (App x y))
rewrite trs (Hole,       nf@(NF x))         = (Hole, nf)
rewrite trs (CApp1 c  t, MApp m1 m2)        = (CApp2 m1 (CApp1 c t), m2)
rewrite trs (CApp1 c  t, mt)                = (c, MApp mt (NF t))
rewrite trs (CApp2 mt c, NF x)              = (CApp1 c x, mt)
rewrite trs (c, MApp m1 m2)                 = rewrite trs (CApp2 m1 c, m2)

nf :: TRS -> Zipper -> Term
nf trs zipper =
    case rewrite trs zipper of
      (Hole, NF x) -> x
      (c,    mt)   -> nf trs (c, mt)
