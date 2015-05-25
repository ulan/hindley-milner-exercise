{-# LANGUAGE DeriveFunctor #-}
module Context (Context,
                Context.lookup,
                empty,
                push,
                values) where

data Context a = Context [a]
  deriving (Eq, Ord, Functor)

empty :: Context a
empty = Context []

push :: a -> Context a -> Context a
push x (Context xs) = Context (x : xs)

lookup :: Integral i => i -> Context a -> Maybe a
lookup i (Context xs)
    | fromIntegral i < length xs = Just (xs !! fromIntegral i)
    | otherwise = Nothing

values :: Context a -> [a]
values (Context xs) = xs