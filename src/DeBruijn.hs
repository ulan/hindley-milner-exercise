module DeBruijn (parse) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import qualified Data.Map as M

import Builtins
import Expr
import Parser
import RawExpr

debruijn :: [(String, Integer)] -> RawExpr -> ErrorT String Identity (Expr (Maybe String))
debruijn builtins = debruijn' (M.fromList (map adjustIndex builtins)) (maxIndex + 1)
  where
    adjustIndex (name, index) = (name, maxIndex - index)
    maxIndex = fromIntegral (length builtins) - 1

debruijn' :: M.Map String Integer -> Integer -> RawExpr -> ErrorT String Identity (Expr (Maybe String))
debruijn' env depth (RawNum n) =
    return $ Num n Nothing
debruijn' env depth (RawBool b) =
    return $ Bool b Nothing
debruijn' env depth (RawVar s) =
    case M.lookup s env of
        Just i -> return $ Var (VarIndex $ depth - 1 - i) Nothing
        Nothing -> throwError ("undefined variable " ++ s)
debruijn' env depth (RawApp x y) = do
    x' <- debruijn' env depth x
    y' <- debruijn' env depth y
    return $ App x' y' Nothing
debruijn' env depth (RawLam x y) = do
    y' <- debruijn' (M.insert x depth env) (depth + 1) y
    return $ Lam y' (Just x) 
debruijn' env depth (RawFix x y) = do
    y' <- debruijn' (M.insert x depth env) (depth + 1) y
    return $ Fix y' (Just x) 
debruijn' env depth (RawLet x y z) = do
    y' <- debruijn' env depth y
    z' <- debruijn' (M.insert x depth env) (depth + 1) z
    return $ Let y' z' (Just x) 

parse :: String -> Either String (Expr (Maybe String))
parse str = runIdentity $ runErrorT (parseExpr str >>= debruijn nameToIndex)
