{-# OPTIONS -Wall #-}
module Arithmetic where

import           Control.Monad.Reader (Reader, local, runReader, reader)
import qualified Data.Map as M

data Expr a = Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Lit a
            | Var String
            | Assign String a (Expr a)

eval :: Num a => M.Map String a -> Expr a -> Maybe a
eval m e = runReader (evalExpr e) m

evalExpr :: Num a => Expr a -> Reader (M.Map String a) (Maybe a)
evalExpr (Add e1 e2) = evalExpr e1 >>= (\x -> evalExpr e2 >>= \y -> reader $ \_ -> (+) <$> x <*> y)
evalExpr (Mul e1 e2) = evalExpr e1 >>= (\x -> evalExpr e2 >>= \y -> reader $ \_ -> (*) <$> x <*> y)
evalExpr (Lit e) = reader $ \_ -> Just e
evalExpr (Var e) = reader $ \env -> M.lookup e env
evalExpr (Assign s v e) = local (M.insert s v) $ evalExpr e

test :: Maybe Int
test = eval (M.insert "x" 1 M.empty ) (Var "x" `Add` (Lit 3 `Mul` ("x" `Assign` 2 $ Var "x")))
