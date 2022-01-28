{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module SimplerEasier.SF where

import Data.List (foldl', union, (\\))
import Data.String (IsString (..))

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Type Expr
  | TLam Name Kind Expr
  | TApp Expr Type
  deriving (Eq, Read, Show)

data Type
  = TVar Name
  | TArrow Type Type
  deriving (Eq, Read, Show)

data Kind
  = Star
  | KArrow Type Type
  deriving (Eq, Read, Show)

