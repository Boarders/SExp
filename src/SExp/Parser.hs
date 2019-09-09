{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE LambdaCase           #-}

module SExp.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Tree
import qualified Text.Megaparsec.Char as C
import Data.Void
import Text.Megaparsec.Char.Lexer (lexeme, decimal)
import Data.Char

type Parser = Parsec Void String

type Symbol = String

data Atom =
    N Int
  | S Symbol
  deriving Show

data SExp =
    A Atom
  | C [SExp]
  deriving Show

lexemeWS :: Parser a -> Parser a
lexemeWS = lexeme C.space

parseInt :: Parser Atom
parseInt = N <$> (lexemeWS decimal)

parseSymbol :: Parser Atom
parseSymbol = S <$> lexemeWS ((:) <$> letterChar <*> takeWhileP Nothing (isAlphaNum))

parseAtom :: Parser Atom
parseAtom = parseInt <|> parseSymbol

parseAtomExp :: Parser SExp
parseAtomExp = A <$> parseAtom

parseBracketExp :: Parser SExp
parseBracketExp =
  do
    _     <- lexemeWS (char ('('))
    sexps <- some parseSExp
    _     <- lexemeWS (char (')'))
    pure $ C sexps
    

parseSExp :: Parser SExp
parseSExp =
  do
    C.space
    parseAtomExp <|> parseBracketExp


toTree :: SExp -> Tree String
toTree = \case
  A atom -> Node (show atom) []
  C ls   -> Node ("") (fmap toTree ls)

pretty :: SExp -> String
pretty = drawTree . toTree
