module Language.SExpr.Type where

import Data.String (IsString (..))
import Text.Pretty (Pretty (..))

data SExpr
  = Atom String
  | List [SExpr]
  | Quote SExpr
  | Unquote SExpr
  deriving stock (Show, Eq)

instance IsString SExpr where
  fromString = Atom

instance Pretty SExpr where
  pretty (Atom x) = x
  pretty (List xs) = "(" <> unwords [pretty x | x <- xs] <> ")"
  pretty (Quote e) = "'" <> pretty e
  pretty (Unquote e) = "," <> pretty e
