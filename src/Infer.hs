module Infer (infer) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import Data.Set (Set, unions, elems, size)
import Debug.Trace

import Builtins
import Context
import Expr (Expr, annotation)
import Type

import qualified Expr

type Inference a = StateT (VarIndex, Substitution) (ErrorT String Identity) a

force :: VarIndex -> Type -> Inference ()
force var typ = do
    (index, subs) <- get
    put (index, compose (substitution [(var, typ)]) subs)

sync :: Type -> Inference Type
sync t = do
    (index, subs) <- get
    return $ Type.apply subs t

unify' :: Type -> Type -> Inference ()
unify' Bool Bool = return ()
unify' Num Num = return ()
unify' (Var x) (Var y)
    | x == y = return ()
    | otherwise = force x (Var y)
unify' (Var x) t
    | t `contains` x = throwError $ "cannot unify " ++ show (Var x) ++ " with " ++ show t
    | otherwise = force x t
unify' t (Var x) = unify (Var x) t
unify' (Arrow t1 t2) (Arrow s1 s2) = do
    unify t1 s1
    unify t2 s2
unify' t s = throwError $ "cannot unify " ++ show t ++ " with " ++ show s

unify :: Type -> Type -> Inference ()
unify t s = do
    t' <- sync t
    s' <- sync s
    unify' t' s'

instantiate :: Schema -> Inference (Substitution, Type)
instantiate schema = do
    let locals = bound schema
    (fresh, subs) <- get
    let fresh' = fresh + (VarIndex $ fromIntegral $ size locals)
    put (fresh', subs)
    let subs = substitution $ zip (elems locals) (map Var [fresh..fresh'])
    return (subs, applyToLocals subs schema)

generalize :: Context Schema -> Type -> Inference Schema
generalize context typ = do
    (_, subs) <- get
    let sync = applyToFree subs
    let freevars = unions (map (free . sync) (values context))
    return $ polymorphic freevars typ

fresh :: Inference Type
fresh = do
    (index, substitution) <- get
    let index' = index + VarIndex 1
    put (index', substitution)
    return $ Var index


typeof :: Expr Typing -> Type
typeof expr = case annotation expr of
    Simple typ -> typ
    Instantiation _ typ -> typ
    Generalization _ typ -> typ

infer' :: Context Schema -> Expr a -> Inference (Expr Typing)
infer' _ (Expr.Bool x _) =
    return $ Expr.Bool x (Simple Bool)
infer' _ (Expr.Num x _) =
    return $ Expr.Num x (Simple Num)
infer' context (Expr.Var x _) =
    case Context.lookup x context of
        Just schema -> do (subs, typ) <- instantiate schema
                          return $ Expr.Var x (Instantiation subs typ)
        Nothing -> throwError $ show (Expr.Var x ()) ++ "is undefined"
infer' context (Expr.Lam expr _) = do
    t <- fresh
    expr' <- infer' (push (monomorphic t) context) expr
    return $ Expr.Lam expr' $ Simple $ Arrow t (typeof expr')
infer' context (Expr.App a b _) = do
    fun <- infer' context a
    s <- fresh
    t <- fresh
    unify (typeof fun) (Arrow s t)
    arg <- infer' context b
    unify s (typeof arg)
    return $ Expr.App fun arg $ Simple t
infer' context (Expr.Fix expr _) = do
    t <- fresh
    expr' <- infer' (push (monomorphic t) context) expr
    unify t (typeof expr')
    return $ Expr.Fix expr' $ Simple t
infer' context (Expr.Let expr1 expr2 _) = do
    expr1' <- infer' context expr1
    let typ = typeof expr1'
    schema <- generalize context typ
    expr2' <- infer' (push schema context) expr2
    return $ Expr.Let expr1' expr2' $ Generalization (bound schema) (typeof expr2')
infer' context (Expr.Builtin index [] _) = do
    (subs, typ) <- instantiate (Builtins.schema index)
    return $ Expr.Builtin index [] $ Instantiation subs typ
infer' context (Expr.Builtin {}) = throwError "Unexpected partially applied builtin"

applyTyping :: Substitution -> Typing -> Typing
applyTyping subs (Simple typ) = Simple (Type.apply subs typ)
applyTyping subs (Instantiation subs1 typ) = Instantiation (applyS subs subs1) (Type.apply subs typ)
applyTyping subs (Generalization locals typ) = Generalization locals (Type.apply subs typ)

infer'' :: Context Schema -> Expr a -> Inference (Expr Typing)
infer'' context expr = do
    expr' <- infer' context expr
    (_, subs) <- get
    return $ fmap (applyTyping subs) expr'

infer :: Context Schema -> Expr a -> Either String (Expr Typing)
infer context expr = runIdentity $ runErrorT $ evalStateT (infer'' context expr) (0, substitution [])
