module Grammar where

import Data.List (intercalate)
import Data.Map
import Data.Sequence
import Data.Maybe


class Calc a where
    calc :: a -> Data.Map.Map String Bool -> Bool

data Binop = Impl | Or | And

instance Ord Binop where
  a `compare` b = (show a) `compare` (show b)

instance Show Binop where
  show Impl = "->"
  show Or   = "|"
  show And  = "&"

instance Calc Expr where
  calc (Binary Impl a b) vals = (not (calc a vals)) || (calc b vals)
  calc (Binary Or a b) vals = (calc a vals) || (calc b vals)
  calc (Binary And a b) vals = (calc a vals) && (calc b vals)
  calc (Not val) vals = not (calc val vals)
  calc (Var name) vals = (fromJust (Data.Map.lookup name vals))
  calc FVal vals = False

instance Eq Binop where
  (==) Impl Impl = True
  (==) Or Or = True
  (==) And And = True
  (==) _ _ = False

data Expr = Binary Binop Expr Expr
          | Not Expr
          | Var String
          | FVal
          deriving (Ord, Eq)

instance Show Expr where
  show (Binary op a b) = "(" ++ intercalate "" [show a, show op, show b] ++ ")"
  show (Not e)         = "(!" ++ show e ++ ")"
  show (Var name)      = name
  show (FVal) = "_|_"


data Context = Context [Expr] deriving (Ord, Eq)

instance Show Context where
  show (Context v) = intercalate "," (Prelude.map show v)
data Line = Line Context Expr deriving(Ord, Eq)

instance Show Line where
  show (Line a b) = show a ++ "|-" ++ show b
