module Pretty (pretty) where

import Control.Monad.Identity
import Control.Monad.Writer
import Control.Applicative
import Data.Char
import qualified Data.Set as S

import Builtins
import Expr
import Type (Type, Typing)
import qualified Type

pretty :: Expr (Maybe String, Typing) -> String
pretty expr = snd $ runIdentity $ runWriterT (pretty' names expr)
 where
  pretty' :: [String] -> Expr (Maybe String, Typing) -> WriterT String Identity ()
  pretty' vars (Num n _) = tell (show n)
  pretty' vars (Bool True _) = tell "true"
  pretty' vars (Bool False _) = tell "false"
  pretty' vars (Var (VarIndex i) (_, Type.Instantiation _ t)) =
    tell (vars !! fromIntegral i)
  pretty' vars (App x y _) = do
    tell "("
    pretty' vars x
    tell ")"
    tell " ("
    pretty' vars y
    tell ")"
  pretty' vars (Lam y (Just name, Type.Simple (Type.Arrow t _))) = do
    tell "\\"
    tell name
    tell " :: "
    tell (prettyType t)
    tell " . "
    pretty' (name:vars) y
  pretty' vars (Fix y (Just name, Type.Simple t)) = do
    tell "fix "
    tell name
    tell " :: "
    tell (prettyType t)
    tell "."
    pretty' (name:vars) y
  pretty' vars (Let y z (Just name, Type.Generalization locals _)) = do
    tell "let "
    tell name
    tell " :: "
    tell (prettyForall locals)
    tell ((prettyType . Type.gettype . snd . annotation) y)
    tell " = ("
    pretty' vars y
    tell ") in "
    pretty' (name:vars) z
  pretty' vars (Builtin index xs _) = do
    tell (name index)
    forM_ xs $ \x -> pretty' vars x

prettyType :: Type -> String
prettyType Type.Bool = "Bool"
prettyType Type.Num = "Number"
prettyType (Type.Var i) = prettyVar i
prettyType (Type.Arrow t1 t2) = "(" ++ prettyType t1 ++ ") -> " ++ prettyType t2

prettyForall :: S.Set (Type.VarIndex) -> String
prettyForall locals = go (S.toList locals)
  where
    go [] = ""
    go xs = "forall " ++ concatMap prettyVar xs ++ " => "

prettyVar :: Type.VarIndex -> String
prettyVar (Type.VarIndex i) = generate !! fromInteger i
  where
    generate = [x : xs | xs <- "" : generate, x <- alphabet]
    alphabet = [chr 0x3B1 .. chr 0x3FF]
