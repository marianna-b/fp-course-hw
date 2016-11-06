{-# OPTIONS -Wall #-}
module SExpr where

import           Data.Char(isSpace, isAlpha, isAlphaNum)
import qualified Control.Applicative as A

import           AParser(Parser, satisfy, posInt)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (:) <$> p <*> zeroOrMore p A.<|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p A.<|> A.empty

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

type Ident = String

data Atom = N Integer | I Ident
  deriving Show

data SExpr = A Atom
    | Comb [SExpr]
    deriving (Show)

someSExprParser :: Parser [SExpr]
someSExprParser = zeroOrMore exprParser

parseAtom :: Parser Atom
parseAtom = (N <$> posInt) A.<|> (I <$> ident)

parseList :: Parser [SExpr]
parseList = satisfy (== '(') *> someSExprParser <* satisfy (== ')')

exprParser :: Parser SExpr
exprParser = spaces *> ((A <$> parseAtom) A.<|> (Comb <$> parseList))
