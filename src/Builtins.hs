module Builtins (arity,
                 close,
                 context,
                 name,
                 names,
                 nameToIndex,
                 reducer,
                 schema) where

import Data.Set (singleton)

import Context
import Expr (Expr, BuiltinIndex(..))
import Type

import qualified Expr

newtype Arity = Arity Int

data Builtin = Builtin String Arity Schema ([Expr ()] -> Expr ())

builtins = [
    Builtin "add" (Arity 2) (monomorphic $ Arrow Num (Arrow Num Num)) add,
    Builtin "sub" (Arity 2) (monomorphic $ Arrow Num (Arrow Num Num)) sub,
    Builtin "mul" (Arity 2) (monomorphic $ Arrow Num (Arrow Num Num)) mul,
    Builtin "cmp" (Arity 2) (monomorphic $ Arrow Num (Arrow Num Bool)) cmp,
    Builtin "if"  (Arity 1) (Schema forall_a $ Arrow Bool (Arrow a (Arrow a a))) if']
  where
    forall_a = singleton (VarIndex 0)
    a = Var (VarIndex 0)
    add [Expr.Num a (), Expr.Num b ()] = Expr.Num (a + b) ()
    sub [Expr.Num a (), Expr.Num b ()] = Expr.Num (a - b) ()
    mul [Expr.Num a (), Expr.Num b ()] = Expr.Num (a * b) ()
    cmp [Expr.Num a (), Expr.Num b ()] = Expr.Bool (a == b) ()
    if' [Expr.Bool True ()]  = Expr.Lam (Expr.Lam (Expr.Var (Expr.VarIndex 1) ()) ()) ()
    if' [Expr.Bool False ()] = Expr.Lam (Expr.Lam (Expr.Var (Expr.VarIndex 0) ()) ()) ()

schema (BuiltinIndex index) =
    let Builtin _ _ s _ = builtins !! fromInteger index in s
name (BuiltinIndex index) =
    let Builtin n _ _ _ = builtins !! fromInteger index in n
arity (BuiltinIndex index) =
    let Builtin _ (Arity a) _ _ = builtins !! fromInteger index in a
reducer (BuiltinIndex index) =
    let Builtin _ _ _ r = builtins !! fromInteger index in r

context :: Context Schema
context = foldr pushvar empty builtins
  where
    pushvar :: Builtin -> Context Schema -> Context Schema
    pushvar (Builtin _ _ schema _) = push schema

close :: Expr () -> Expr ()
close expr = foldl apply expr [0..fromIntegral $ length builtins - 1]
  where
    apply :: Expr () -> Integer -> Expr ()
    apply expr index = Expr.Let (Expr.Builtin (BuiltinIndex index) [] ()) expr ()

nameToIndex :: [(String, Integer)]
nameToIndex = zipWith var [0..] builtins
  where var i (Builtin name _ _ _) = (name, i)

names :: [String]
names = map (name . BuiltinIndex . fromIntegral) [0 .. length builtins - 1]
