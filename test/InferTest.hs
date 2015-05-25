module InferTest (tests) where

import Control.Monad
import Test.HUnit

import Builtins
import Context
import DeBruijn
import Infer
import Parser
import Type
import TypeChecker

import qualified Expr

compute :: String -> Either String Bool
compute str = do
    expr <- parse str
    typed <- infer context (void expr)
    return $ check context typed

typed str = Right True ~=? compute str

nottyped str = case compute str of
    Left s -> True ~? "all right"
    Right _ -> False ~? "expected not typeable"

tests = test [
    typed "\\x . x",
    typed "\\x . add x (mul 1 3)",
    typed "\\x . add x (mul 1 3)",
    typed "\\x . if (cmp x (mul 1 3)) 1 3",
    typed "\\x . if (cmp x (mul 1 3)) 1 3",
    typed "let id = (\\x . x) in id 1",
    typed "let id = (\\x . x) in id id",
    typed "(fix f . \\n . if (cmp n 0) 1 (if (cmp n 1) 1 (add (f (sub n 1)) (f (sub n 2))))) 2",
    nottyped "\\x . x x"]