{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module SimplerEasier.Pi where

import Data.List (foldl', union, (\\))
import Data.String (IsString (..))

type Name = String

type Type = Expr

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Type Expr
  | Pi Name Type Type
  | Kind Kinds
  deriving (Eq, Read, Show)

data Kinds = Star | Box
  deriving (Eq, Read, Show)

freeVars :: Expr -> [Name]
freeVars (Var s) = [s]
freeVars (App f e) = freeVars f `union` freeVars e
freeVars (Lam s t e) = freeVars t `union` (freeVars e \\ [s])
freeVars (Pi s t e) = freeVars t `union` (freeVars e \\ [s])
freeVars (Kind _) = []

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
    -- these two are identical
    go (Lam s' t ex) = abstract Lam s' t ex
    go (Pi s' t ex) = abstract Pi s' t ex
    go (Kind k) = Kind k

    -- find a name similar to s that is not free in e or x
    cloneSym ex s = loop s
      where
        loop s'
          | s' `elem` vars = loop (s ++ "'")
          | otherwise = s'
        vars = freeVars x ++ freeVars ex

    abstract con s' t ex
      -- lambda term shadows our s, we have nothing to do here
      | s == s' = con s t ex
      -- lambda term shadows a free name in x!
      | s' `elem` freeVars x =
        let s'' = cloneSym ex s'
            -- new name s'' for s' that is not free in ex or x
            ex' = subst s' (Var s'') ex
         in -- new body with s'' substituted for s'
            con s'' (go t) (go ex')
      -- lambda term is safe to go through
      | otherwise = con s' (go t) (go ex)

nf :: Expr -> Expr
nf ex = spine ex []
  where
    -- if we find an App, we stash it in as
    spine (App f e) as =
      spine f (nf e : as)
    -- if we find a Lam and as is not empty then we have a redex
    -- we consume an a off as, do the substitution and recurse on the
    -- resulting term (exactly like above)
    spine (Lam s _ e) (a : as) =
      spine (subst s a e) as
    -- for normal form we need to recurse under the lambda
    spine (Lam s t e) [] =
      Lam s t (nf e)
    spine (Pi s k t) as =
      foldl' App (Pi s (nf k) (nf t)) as
    spine v@(Var _) as =
      foldl' App v as
    spine k@(Kind _) as =
      foldl' App k as

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var s) (Var s') = s == s'
alphaEq (App f e) (App f' e') = alphaEq f f' && alphaEq e e'
alphaEq (Lam s t e) (Lam s' t' e') = alphaEq e (subst s' (Var s) e') && alphaEq t t'
alphaEq (Pi s t e) (Pi s' t' e') = alphaEq e (subst s' (Var s) e') && alphaEq t t'
alphaEq _ _ = False

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

newtype Env = Env [(Name, Type)]
  deriving (Show)

emptyEnv :: Env
emptyEnv = Env []

pushEnv :: Name -> Type -> Env -> Env
pushEnv s t (Env r) = Env ((s, t) : r)

type ErrorMsg = String

type TC a = Either ErrorMsg a

findVar :: Env -> Name -> TC Type
findVar (Env r) s =
  case lookup s r of
    Just t -> return t
    Nothing -> Left $ "Cannot find variable " ++ show s

typeCheck' :: Env -> Expr -> TC Type
typeCheck' r (Var s) =
  findVar r s
typeCheck' r (App f a) =
  do
    tf <- typeCheck' r f
    case tf of
      Pi x at rt ->
        do
          ta <- typeCheck' r a
          if ta `betaEq` rt
            then Right $ subst x at rt
            else Left $ "Bad function argument type: function " ++ show f ++ " takes a value of type " ++ show at ++ " but argument " ++ show a ++ " has type " ++ show ta
      _ ->
        Left $ "Non-function application: term " ++ show f ++ " has type " ++ show tf ++ " which is not a function type"
typeCheck' r (Lam s t e) =
  do
    typeCheck' r t
    let r' = pushEnv s t r
    te <- typeCheck' r' e
    let lt = Pi s t te
    typeCheck' r lt
    return lt
typeCheck' r (Pi s t e) =
  do
    ts <- typeCheck' r t
    let r' = pushEnv s t r
    te <- typeCheck' r' e
    if (ts, te) `notElem` allowedKinds
      then Left "Bad abstractions"
      else Right t
typeCheck' r (Kind Star) = Right (Kind Box)
typeCheck' r (Kind Box) = Left "Found a box"

allowedKinds :: [(Type, Type)]
allowedKinds =
  [ (Kind Star, Kind Star),
    (Kind Star, Kind Box),
    (Kind Box, Kind Star),
    (Kind Box, Kind Box)
  ]

typeCheck :: Expr -> TC Type
typeCheck = typeCheck' emptyEnv
