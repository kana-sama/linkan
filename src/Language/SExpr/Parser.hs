module Language.SExpr.Parser (parse, parses) where

import Data.Void (Void)
import Language.SExpr.Type (SExpr (..))
import Text.Megaparsec (Parsec, between, choice, empty, eof, many, manyTill, oneOf, runParser, some, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

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
    atom = lexeme (some (alphaNumChar <|> oneOf ("+-*/><!?=&" :: [Char])))
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
