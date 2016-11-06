{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser
    ( Expr(..)
    , BinOp(..)
    , expr
    , varList
    ) where

import           Control.Monad          (void)
import qualified Text.Megaparsec        as M
import qualified Text.Megaparsec.Expr   as ME
import qualified Text.Megaparsec.Lexer  as L
import qualified Text.Megaparsec.String as MS

data Expr a = Expr BinOp (Expr a) (Expr a)
            | Neg (Expr a)
            | Lit a
            | Var String
            deriving Show

data BinOp = Add | Sub | Mul | Div | Pow
  deriving Show

sc :: MS.Parser ()
sc = L.space (void M.spaceChar) (void M.spaceChar) (void M.spaceChar)

integer :: MS.Parser Integer
integer = lexeme L.integer

lexeme :: MS.Parser a -> MS.Parser a
lexeme = L.lexeme sc

symbol :: String -> MS.Parser String
symbol = L.symbol sc

parens :: MS.Parser a -> MS.Parser a
parens = M.between (symbol "(") (symbol ")")

identifier :: MS.Parser String
identifier = lexeme p
  where p = (:) <$> M.letterChar <*> M.many M.alphaNumChar

term :: MS.Parser (Expr Integer)
term = parens expr M.<|> Var <$> identifier M.<|> Lit <$> integer

expr :: MS.Parser (Expr Integer)
expr = ME.makeExprParser term operators

operators :: [[ME.Operator MS.Parser (Expr Integer)]]
operators =
  [ [ME.Prefix (symbol "-" *> pure Neg) ]
  , [ ME.InfixR (symbol "^" *> pure (Expr Pow))]
  , [ ME.InfixL (symbol "*" *> pure (Expr Mul))
    , ME.InfixL (symbol "/" *> pure (Expr Div)) ]
  , [ ME.InfixL (symbol "+" *> pure (Expr Add))
    , ME.InfixL (symbol "-" *> pure (Expr Sub)) ]
  ]

var :: MS.Parser (String, Integer)
var = parens $ (,) <$> identifier <* symbol "," <*> integer

varList :: MS.Parser [(String, Integer)]
varList = M.many var
