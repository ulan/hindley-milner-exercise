{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}

module Expr where

import Prelude hiding (zip)

newtype VarIndex = VarIndex Integer
  deriving (Eq, Ord, Show, Num, Enum, Integral, Real)

newtype BuiltinIndex = BuiltinIndex Integer
  deriving (Eq, Ord, Show, Num, Enum, Integral, Real)

data Expr a = Bool Bool a
            | Num Integer a
            | Var VarIndex a
            | Lam (Expr a) a
            | App (Expr a) (Expr a) a
            | Fix (Expr a) a
            | Let (Expr a) (Expr a) a
            | Builtin BuiltinIndex [Expr a] a
  deriving (Eq, Ord, Show, Functor)

annotation :: Expr a -> a
annotation (Bool _ x) = x
annotation (Num _ x) = x
annotation (Var _ x) = x
annotation (Lam _ x) = x
annotation (App _ _ x) = x
annotation (Fix _ x) = x
annotation (Let _ _ x) = x
annotation (Builtin _ _ x) = x

zip :: Expr a -> Expr b -> Expr (a, b)
zip (Bool x a) (Bool _ b) = Bool x (a, b)
zip (Num x a) (Num _ b) = Num x (a, b)
zip (Var x a) (Var _ b) = Var x (a, b)
zip (Lam x a) (Lam x' b) =
    Lam (zip x x') (a, b)
zip (App x y a) (App x' y' b) =
    App (zip x x') (zip y y') (a, b)
zip (Fix x a) (Fix x' b) =
    Fix (zip x x') (a, b)
zip (Let x y a) (Let x' y' b) =
    Let (zip x x') (zip y y') (a, b)
zip (Builtin i xs a) (Builtin _ xs' b) =
    Builtin i (zipWith zip xs xs') (a, b)

