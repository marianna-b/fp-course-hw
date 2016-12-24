{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}
module Template
  ( selN
  , getCompileEnv
  , fib
  , prettyShow
  ) where

import           Control.Monad              (liftM)
import           Data.List                  (intercalate)
import           Language.Haskell.TH        (Con (..), Dec (..), Exp (..), ExpQ,
                                             Info (..), Name, Pat (..), Q, conT, listE,
                                             nameBase, newName, reify, runIO, varE)
import           Language.Haskell.TH.Syntax (Lift (..))
import           System.Environment         (getEnvironment)

selN :: Int -> Int -> Q Exp
selN n i = do
   x <- newName "x"
   return $ LamE [TupP (replicate (i - 1) WildP ++ (VarP x : replicate (n - i) WildP))] (VarE x)

getCompileEnv :: String -> Q Exp
getCompileEnv key = do
  l <- lookup key `liftM` runIO getEnvironment
  case l of
    Nothing -> lift ""
    Just x  -> lift x

fib' :: Int -> (Int, Int) -> Int
fib' !n (!a, !b) | n == 0    = a
                 | otherwise = fib' (n - 1) (b, a + b)

fib :: Int -> ExpQ
fib n = [| x |]
  where x = fib' n (0, 1)

showField :: Name -> Q Exp
showField name = [| \x -> s ++ " = " ++ show ($(varE name) x) |]
  where s = nameBase name

showFields :: [Name] -> Q Exp
showFields names = listE $ map showField names

prettyShow :: Name -> Q [Dec]
prettyShow name = do
    TyConI (DataD _ _ _ _ [RecC _ fields] _ ) <- reify name
    let names = map (\(n,_,_) -> n) fields
    [d|instance Show $(conT name) where show x = s ++ " {\n" ++ intercalate ",\n" (map (\y -> "    " ++ ($ x) y) $(showFields names)) ++ "\n}"|]
      where s = nameBase name
