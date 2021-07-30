{-# LANGUAGE PatternSynonyms #-}

module Language.Linkan.Value (Value (..), pattern ValueList) where

import Data.Map.Strict (Map)
import Data.String (IsString (..))
import Language.SExpr.Type (SExpr)
import Text.Pretty (Pretty (..))

data Value
  = Cons Value Value
  | Symbol String
  | Closure (Map String Value) [String] SExpr
  deriving stock (Show, Eq)

instance IsString Value where
  fromString = Symbol

valueToList :: Value -> Maybe ([Value], Maybe Value)
valueToList xs =
  case xs of
    "nil" -> Just ([], Nothing)
    Cons {} -> Just (go xs)
    _ -> Nothing
  where
    go "nil" = ([], Nothing)
    go (Cons a b) = let (xs, last) = go b in (a : xs, last)
    go x = ([], Just x)

pattern ValueList xs x <- (valueToList -> Just (xs, x))

instance Pretty Value where
  pretty (ValueList ["quote", e] Nothing) = "'" <> pretty e
  pretty (ValueList ["unquote", e] Nothing) = "," <> pretty e
  pretty (ValueList xs Nothing) = "(" <> unwords [pretty x | x <- xs] <> ")"
  pretty (ValueList xs (Just x)) = "(" <> unwords [pretty x | x <- xs] <> " . " <> pretty x <> ")"
  pretty (Cons a b) = "(cons " <> pretty a <> " " <> pretty b <> ")"
  pretty (Symbol x) = x
  pretty (Closure _ _ _) = "(lambda (..) ..)"
