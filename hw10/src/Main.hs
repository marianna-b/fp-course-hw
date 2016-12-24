{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens      ((%~), (&), (.~), (^.), (^?))
import           Control.Lens.Fold ((^..))
import           Lens
import           Template

data MyData = MyData
    { foo :: String
    , bar :: Int
    }

prettyShow ''MyData

main :: IO ()
main = do
  print $ $(selN 4 1) ("hello", 1, [4, 3], 2)
  print $ $(selN 4 3) ("hello", 1, [4, 3], 2)
  print $(getCompileEnv "TH_ENV")
  print $(fib 24)
  print $ MyData { foo = "bar", bar = 5 }
  l <- getDir "."
  print $ fileName l
  print $ fileNameMaybe l
  print $ fileName $ File "test"
  print $ fileNameMaybe $ File "test"
  print l
  print $ newRoot "ololo" l
  print $ firstDir l
  print $ addPrefix "boooo" l
  print $ l^..ls
  print $ l^.. cd "src" . ls
  print $ l^.. cd "src" . file "Main.hs"
  print $ l^? cd "src" . file "bbb"
  print $ ("", Dir "ololo" []) ^. move "A" . move "B" . getPath
  print $ changeExtInDir ".h" l
