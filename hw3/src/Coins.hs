module Coins where

newtype Coin color = Coin { getCoin :: Integer }
  deriving Show

instance Monoid (Coin a) where
  mempty = Coin 0
  mappend (Coin a) (Coin b) = Coin (a + b)

instance Num (Coin a) where
  (+) (Coin a) (Coin b) = Coin (a + b)
  (-) (Coin a) (Coin b) = Coin (a - b)
  (*) (Coin a) (Coin b) = Coin (a * b)
  abs (Coin a) = Coin $ abs a
  signum (Coin a)
    | a > 0 = Coin 1
    | a < 0 = Coin $ -1
    | otherwise = Coin 0
  fromInteger = Coin

data Blue
data Red

red = undefined :: Red
blue = undefined :: Blue

class Color a where
  getValue :: a -> Int

instance Color Red where
  getValue _ = 1

instance Color Blue where
  getValue _ = 2

cmp :: (Color color1, Color color2) => color1 -> color2 -> Coin color1 -> Coin color2 -> Int
cmp c1 c2 (Coin a) (Coin b)
  | getValue c1 < getValue c2 = -1
  | getValue c1 > getValue c2 = 1
  | a > b = 1
  | a < b = -1
  | otherwise = 0
