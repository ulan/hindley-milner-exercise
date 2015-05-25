module Parser (parseExpr) where


import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import Control.Applicative hiding ((<|>), many)
import Data.Char
import Text.ParserCombinators.Parsec

import RawExpr

expr :: Parser RawExpr
expr = do
    xs <- separatedBy spaces (paren <|> lambda <|> rest)
    return (apply xs)
  where
    paren = do
        char '('
        inner <- expr
        char ')'
        return inner
    lambda  = do
        char '\\'
        spaces
        vars <- separatedBy spaces identifier
        spaces
        char '.'
        spaces
        body <- expr
        return (abstract vars body)
    rest = do
        x <- atom
        case x of
            "fix" -> fixx
            "let" -> lett
            "true" -> return $ RawBool True
            "false" -> return $ RawBool False
            otherwise -> return $ numOrVar x
    fixx = do
        spaces
        var <- identifier
        spaces
        char '.'
        spaces
        body <- expr
        return $ RawFix var body
    lett = do
        spaces
        var <- identifier
        spaces
        char '='
        spaces
        char '('
        spaces
        value <- expr
        spaces
        char ')'
        spaces
        string "in"
        spaces
        body <- expr
        return $ RawLet var value body
    apply (x : xs) = foldl RawApp x xs
    abstract vars body = foldr RawLam body vars
    identifier = many1 (letter <|> digit <|> oneOf "_$'")
    atom = many1 (letter <|> digit <|> oneOf "_$'")
    separatedBy sep item = many1 (item <* sep)
    numOrVar x | all isDigit x = let i = (read x :: Integer) in RawNum i
               | otherwise = RawVar x

parseExpr :: String -> ErrorT String Identity RawExpr
parseExpr str =
    case parse expr "" str of
      Left err -> throwError $ show err
      Right x -> return x