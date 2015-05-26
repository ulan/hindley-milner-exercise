module EvalTest (tests) where

import Control.Monad
import Test.HUnit

import Builtins
import DeBruijn
import Eval
import Expr
import Infer

compute :: String -> Either String (Expr ())
compute str = do
    expr <- parse str
    infer context (void expr)
    return $ eval (void expr)

expect n str = Right (Num n ()) ~=? compute str

tests = test [
    expect 1 "(\\x . x) 1",
    expect 1 "(let id = (\\y . y) in id id) 1",
    expect 5 "(\\n . if (cmp n 0) 2 5) 1",
    expect 8 "(fix fibo . (\\n . if (cmp n 0) 0 (if (cmp n 1) 1 (add (fibo (sub n 1)) (fibo (sub n 2)) ) ))) 6",
    expect 120 "(fix fac . (\\n . if (cmp n 0) 1 (mul (fac (sub n 1)) n))) 5",
    expect 3628800 "(fix fact . \\x . if (cmp x 1) 1 (mul x (fact (sub x 1)))) 10",
    expect 5 "let mod = (\\x . \\m . (sub x (mul (div x m) m))) in mod 15 10",
    expect 5 "let mod = (\\x . \\m . (sub x (mul (div x m) m))) in mod 5 10",
    expect 7 "let mod = (\\x . \\m . (sub x (mul (div x m) m))) in \
             \(fix gcd . \\ x . \\ y . if (cmp x 0) y (gcd (mod y x) x)) 49 35"
    ]
