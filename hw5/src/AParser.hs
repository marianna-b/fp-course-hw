{-# OPTIONS -Wall #-}
module AParser
    ( Parser (runParser)
    , satisfy
    , posInt
    , abParser
    , abParser_
    , intOrUppercase
    ) where

import           Data.Char(isDigit, isUpper)
import qualified Control.Monad as M
import qualified Control.Applicative as A

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
          | p x = Just (x, xs)
          | otherwise = Nothing

posInt :: Parser Integer
posInt = Parser f
  where f xs
          | null ns = Nothing
          | otherwise = Just (read ns, rest)
          where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, b) = (f a, b)

instance Functor Parser where
  fmap f (Parser l) = Parser $ \s -> first f <$> l s

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  Parser f <*> Parser x = Parser $ f M.>=> apply
    where apply (func, str) = fmap (first func) (x str)

abParser :: Parser (Char, Char)
abParser = (,) <$> satisfy (=='a') <*> satisfy (=='b')

abParser_ :: Parser ()
abParser_ = M.void abParser

intPair :: Parser [Integer]
intPair = (\x _ y -> x:[y]) <$> posInt <*> satisfy (== ' ') <*> posInt

instance A.Alternative Parser where
  empty                 = Parser $ const Nothing
  Parser a <|> Parser b = Parser $ \s -> a s A.<|> b s

intOrUppercase :: Parser ()
intOrUppercase = M.void posInt A.<|> M.void (satisfy isUpper)
