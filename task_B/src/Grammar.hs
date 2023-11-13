module Grammar where

import Data.List (intercalate)

data Binop = Impl | Or | And

instance Ord Binop where
  a `compare` b = (show a) `compare` (show b)

instance Show Binop where
  show Impl = "->"
  show Or   = "|"
  show And  = "&"

instance Eq Binop where
  (==) Impl Impl = True
  (==) Or Or = True
  (==) And And = True
  (==) _ _ = False

data Expr = Binary Binop Expr Expr
          | Not Expr
          | Var String
          deriving (Ord, Eq)

instance Show Expr where
  show (Binary op a b) = "(" ++ intercalate "" [show a, show op, show b] ++ ")"
  show (Not e)         = "(!" ++ show e ++ ")"
  show (Var name)      = name


data Context = Context [Expr] deriving (Ord, Eq)

instance Show Context where
  show (Context v) = intercalate "," (map show v)
data Line = Line Context Expr deriving(Ord, Eq)

instance Show Line where
  show (Line a b) = show a ++ "|-" ++ show b
