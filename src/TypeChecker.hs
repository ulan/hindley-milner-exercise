module TypeChecker (check) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Writer
import Data.Set (Set, unions, elems, size, intersection)

import Builtins
import Context
import Expr (Expr, annotation)
import Type

import qualified Expr

typeof :: Expr Typing -> Type
typeof expr = case annotation expr of
    Simple typ -> typ
    Instantiation _ typ -> typ
    Generalization _ typ -> typ

check :: Context Schema -> Expr Typing -> Bool
check _ (Expr.Bool _ (Simple Bool)) = True
check _ (Expr.Num _ (Simple Num)) = True
check context (Expr.Var x (Instantiation subs typ)) =
    case Context.lookup x context of
        Just schema -> keys subs == bound schema &&
                       applyToLocals subs schema == typ
        Nothing -> False
check context (Expr.Lam expr (Simple (Arrow s t))) =
  check (push (monomorphic s) context) expr && typeof expr == t
check context (Expr.App a b (Simple typ)) =
    check context a && check context b &&
    case typeof a of
        Arrow s t -> s == typeof b && t == typ
        _         -> False
check context (Expr.Fix expr (Simple typ)) =
  check (push (monomorphic typ) context) expr && typeof expr == typ
check context (Expr.Let expr1 expr2 (Generalization locals typ)) =
   size (intersection locals $ unions (map free (values context))) == 0 &&
   check (push (Schema locals (typeof expr1)) context) expr2 &&
   typeof expr2 == typ
check context (Expr.Builtin index [] (Instantiation subs typ)) =
    let schema = Builtins.schema index in
    keys subs == bound schema && applyToLocals subs schema == typ
check _ _ = False
