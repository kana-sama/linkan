module Text.Pretty (Pretty (..)) where

class Pretty a where
  pretty :: a -> String
