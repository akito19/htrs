module HTRS (showResult, parseTRS) where

import HTRS.Term
import HTRS.Parser
import HTRS.Rewrite
import HTRS.Type

-- Initial Type Environment
env0 :: Env
env0 = [
  ("true",  TCon "Bool"),
  ("false", TCon "Bool"),
  ("0",     TCon "Nat"),
  ("s",     TArr (TCon "Nat") (TCon "Nat")),
  ("nil",   TApp (TCon "List") (TVar "a")),
  ("cons",  TArr (TVar "a") (TArr (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a"))))]

showEnv :: Env -> String
showEnv []              = ""
showEnv ((str, t) : es) = str ++ " :: " ++ show t ++ ";" ++ "\n" ++ showEnv es

showResult :: TRS -> String
showResult trs = typeStr ++ "\n" ++ calcNF
    where
        typeStr = showEnv $ signature env0 trs
        calcNF  = show $ nf trs (Hole, (MCon "main"))
