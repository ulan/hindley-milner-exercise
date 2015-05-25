module RawExpr where

data RawExpr = RawBool Bool
             | RawNum Integer
             | RawVar String
             | RawApp RawExpr RawExpr
             | RawLam String RawExpr
             | RawLet String RawExpr RawExpr
             | RawFix String RawExpr
  deriving (Eq, Show)
