{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module SimplerEasier where

import Data.List (foldl', union, (\\))
import Data.String (IsString (..))

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  deriving (Eq, Read, Show)

instance IsString Expr where
  fromString = Var

whnf :: Expr -> Expr
whnf ex = spine ex []
  where
    -- if we find an App, we evaluate it to nf and stash it in as
    spine (App f e) as =
      spine f (e : as)
    -- if we find a Lam and as is not empty then we have a redex
    -- we consume an a off as, do the substitution and recurse on the
    -- resulting term
    spine (Lam s e) (a : as) =
      spine (subst s a e) as
    -- here we leave the lambdas alone
    spine (Lam s e) [] =
      Lam s e
    spine v@(Var _) as =
      foldl' App v as

nf :: Expr -> Expr
nf ex = spine ex []
  where
    -- if we find an App, we stash it in as
    spine (App f e) as =
      spine f (nf e : as)
    -- if we find a Lam and as is not empty then we have a redex
    -- we consume an a off as, do the substitution and recurse on the
    -- resulting term (exactly like above)
    spine (Lam s e) (a : as) =
      spine (subst s a e) as
    -- for normal form we need to recurse under the lambda
    spine (Lam s e) [] =
      Lam s (nf e)
    spine v@(Var _) as =
      foldl' App v as

freeVars :: Expr -> [Name]
freeVars (Var s) = [s]
freeVars (App f e) = freeVars f `union` freeVars e
freeVars (Lam s e) = freeVars e \\ [s]

-- subs s x e computes e[s:=x]
subst :: Name -> Expr -> Expr -> Expr
subst s x = go
  where
    -- if we find a matching var we replace it otherwise we leave it alone
    go v@(Var s')
      | s == s' = x
      | otherwise = v
    -- recurse inside both the function and the argument
    go (App ex ex') =
      App (go ex) (go ex')
    go l@(Lam s' ex)
      -- lambda term shadows our s, we have nothing to do here
      | s == s' = l
      -- lambda term shadows a free name in x!
      | s' `elem` freeVars x =
        let s'' = cloneSym ex s'
            -- new name s'' for s' that is not free in ex or x
            ex' = subst s' (Var s'') ex
         in -- new body with s'' substituted for s'
            Lam s'' (go ex')
      -- lambda term is safe to go through
      | otherwise = Lam s' (go ex)

    -- find a name similar to s that is not free in e or x
    cloneSym ex s = loop s
      where
        loop s'
          | s' `elem` vars = loop (s ++ "'")
          | otherwise = s'
        vars = freeVars x ++ freeVars ex

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var s) (Var s') = s == s'
alphaEq (App f e) (App f' e') = alphaEq f f' && alphaEq e e'
alphaEq (Lam s e) (Lam s' e') = alphaEq e (subst s' (Var s) e')
alphaEq _ _ = False

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

instance Num Expr where
  fromInteger n = Lam "s" $ Lam "z" $ iterate (App "s") "z" !! fromIntegral n
  (+) = error "not implemented"
  (*) = error "not implemented"
  abs = error "not implemented"
  signum = error "not implemented"
  negate = error "not implemented"

plus :: Expr
plus =
  Lam "m" $
    Lam "n" $
      Lam "s" $
        Lam "z" $
          "m" `App` "s" `App` ("n" `App` "s" `App` "z")
