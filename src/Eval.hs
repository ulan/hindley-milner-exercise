module Eval (eval) where

import Control.Applicative

import Builtins
import Expr

mapfree :: (Integer -> Integer -> Expr ()) -> Expr () -> Expr ()
mapfree f = replace 0
  where
    replace depth (Var (VarIndex i) _) | i < depth = Var (VarIndex i) ()
                                       | otherwise = f depth i
    replace depth (App x y _) = App (replace depth x) (replace depth y) ()
    replace depth (Lam y _) = Lam (replace (depth + 1) y) ()
    replace depth (Let x y _) = Let (replace depth x) (replace (depth + 1) y) ()
    replace depth (Fix x _) = Fix (replace (depth + 1) x) ()
    replace depth (Builtin index xs _) = Builtin index (map (replace depth) xs) ()
    replace depth x = x

add :: Integer -> Expr () -> Expr ()
add n = mapfree (\depth i -> Var (VarIndex (i + n)) ())

subst :: Expr () -> Expr () -> Expr ()
subst z = mapfree (\depth i -> if i == depth then add (depth + 1) z else Var (VarIndex i) ())

beta :: Expr () -> Maybe (Expr ())
beta (Num n _) = Nothing
beta (Bool b _) = Nothing
beta (Var _ _) = Nothing
beta (App (Lam y _) z _ ) = Just $ add (-1) $ subst z y
beta (App (Builtin index xs _) z _ ) =
  ((\a -> App (Builtin index xs ()) a ()) <$> beta z) <|>
    if length xs + 1 == arity index
      then Just $ reducer index (xs ++ [z])
      else Just $ Builtin index (xs ++ [z]) ()
beta (App x z _) = ((\a -> App a z ()) <$> beta x) <|> ((\a -> App x a ()) <$> beta z)
beta (Lam y _) = Nothing
beta (Let x y _) = Just $ add (-1) $ subst x y
beta (Fix x _) = Just $ add (-1) $ subst (Fix x ()) x
beta (Builtin {}) = Nothing

reduce :: (a -> Maybe a) -> a -> [a]
reduce rule initial = case rule initial of
  Just x -> x : reduce rule x
  Nothing -> []

reductions :: Expr () -> [Expr ()]
reductions expr = reverse $ expr : reduce beta expr

eval :: Expr () -> Expr ()
eval expr = head $ reductions $ close expr
