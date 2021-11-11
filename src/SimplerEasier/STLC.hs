{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module SimplerEasier.STLC where

import Data.List (foldl', union, (\\))
import Data.String (IsString (..))

type Name = String

data Type
  = Base
  | Arrow Type Type
  deriving (Eq)

instance Show Type where
  show Base = "*"
  show (Arrow t1 t2) = show t1 ++ " -> " ++ show t2

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Type Expr
  deriving (Eq, Show)

instance IsString Expr where
  fromString = Var

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
      Arrow at rt ->
        do
          ta <- typeCheck' r a
          if ta /= at
            then Left $ "Bad function argument type: function " ++ show f ++ " takes a value of type " ++ show at ++ " but argument " ++ show a ++ " has type " ++ show ta
            else Right rt
      Base ->
        Left $ "Non-function application: term " ++ show f ++ " has type " ++ show tf ++ " which is not a function type"
typeCheck' r (Lam s t e) =
  do
    let r' = pushEnv s t r
    te <- typeCheck' r' e
    return (Arrow t te)

typeCheck :: Expr -> TC Type
typeCheck = typeCheck' emptyEnv
