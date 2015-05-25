{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type (Schema (..),
             Substitution,
             Type (..),
             Typing (..),
             VarIndex (..),
             apply,
             applyS,
             applyToFree,
             applyToLocals,
             bound,
             compose,
             contains,
             free,
             gettype,
             keys,
             monomorphic,
             polymorphic,
             substitution) where

import Data.Set (Set, empty, singleton, union, difference, member, fromList)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

newtype VarIndex = VarIndex Integer
  deriving (Eq, Ord, Show, Num, Enum)

data Type = Bool
          | Num
          | Var VarIndex
          | Arrow Type Type
  deriving (Eq, Ord, Show)

data Schema = Schema (Set VarIndex) Type
  deriving (Eq, Ord, Show)

type Substitution = M.Map VarIndex Type

data Typing = Simple Type
            | Instantiation Substitution Type
            | Generalization (Set VarIndex) Type
  deriving (Show)

gettype (Simple t) = t
gettype (Instantiation _ t) = t
gettype (Generalization _ t) = t

substitution :: [(VarIndex, Type)] -> Substitution
substitution = M.fromList

mapping :: Substitution -> VarIndex -> Type
mapping subs x = fromMaybe (Var x) (M.lookup x subs)

compose :: Substitution -> Substitution -> Substitution
compose subs1 subs2 = M.union subs1 $ applyS subs1 subs2

applyS :: Substitution -> Substitution -> Substitution
applyS subs = M.map (Type.apply subs)

vars :: Type -> Set VarIndex
vars Bool = empty
vars Num = empty
vars (Var x) = singleton x
vars (Arrow t1 t2) = vars t1 `union` vars t2

contains :: Type -> VarIndex -> Bool
contains typ var = member var (vars typ)

apply :: Substitution -> Type -> Type
apply subs Bool = Bool
apply subs Num = Num
apply subs (Var x) = mapping subs x
apply subs (Arrow t1 t2) = Arrow (apply subs t1) (apply subs t2)

applyToFree :: Substitution -> Schema -> Schema
applyToFree subs (Schema locals typ) = Schema locals (apply subs' typ)
  where
      subs' = M.filterWithKey (\k v -> not (k `member` locals)) subs

applyToLocals :: Substitution -> Schema -> Type
applyToLocals subs (Schema locals typ) = apply subs' typ
  where
      subs' = M.filterWithKey (\k v -> k `member` locals) subs

keys :: Substitution -> Set VarIndex
keys subs = fromList $ M.keys subs

bound :: Schema -> Set VarIndex
bound (Schema locals _) = locals

free :: Schema -> Set VarIndex
free (Schema locals typ) = difference (vars typ) locals

monomorphic :: Type -> Schema
monomorphic = Schema empty

polymorphic :: Set VarIndex -> Type -> Schema
polymorphic free typ = Schema locals typ
  where locals = difference (vars typ) free

