module Main where

import Control.Monad
import Control.Applicative
import Test.HUnit

import EvalTest
import InferTest

import System.Exit (exitFailure)

check tests = do
    (Counts _ _ errors failures) <- runTestTT tests
    if (errors + failures > 0)
        then exitFailure
        else return $ ()


main = do
    check (InferTest.tests)
    check (EvalTest.tests)