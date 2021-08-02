{-# LANGUAGE ImportQualifiedPost #-}

module Language.SExpr (SExpr (..), parse, parses) where

import Data.String (IsString (..))
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, choice, empty, eof, many, manyTill, oneOf, runParser, some, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (errorBundlePretty)
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

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

sexpr :: Parser SExpr
sexpr =
  choice
    [ Atom <$> atom <?> "atom",
      List <$> list <?> "list",
      Quote <$> quote <?> "quote",
      Unquote <$> unquote <?> "unquote"
    ]
  where
    atom = lexeme (some (alphaNumChar <|> oneOf ("+-*/><!?=&._" :: [Char])))
    list = between (symbol "(") (symbol ")") (many sexpr)
    quote = char '\'' *> sexpr
    unquote = char ',' *> sexpr

parse :: String -> SExpr
parse source =
  case runParser (sc *> sexpr <* eof) "-" source of
    Left err -> error (errorBundlePretty err)
    Right value -> value

parses :: String -> [SExpr]
parses source =
  case runParser (sc *> many sexpr <* eof) "-" source of
    Left err -> error (errorBundlePretty err)
    Right value -> value
