module Main where

import Control.Monad

import Builtins
import Context
import DeBruijn
import Eval
import Expr (Expr)
import Infer
import Parser
import Type
import TypeChecker
import Pretty


import qualified Expr

compute :: String -> Either String (Expr (Maybe String, Typing), Expr ())
compute str = do
    expr <- parse str
    typed <- infer context (void expr)
    if not (check context typed)
        then Left "type inference failed"
        else return (Expr.zip expr typed, eval (void expr))

main = do
    str <- getContents
    case compute str of
        Left err -> putStrLn err
        Right (typed, result) -> do
            putStrLn (pretty typed)
            print result