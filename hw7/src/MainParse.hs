{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where

import           Control.Arrow         (second)
import           Control.Exception     (Exception, catch, throw)
import           Control.Monad.Reader  (Reader, reader, runReader)
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import           Data.Typeable         (Typeable)
import qualified Options.Applicative   as O
import qualified Text.Megaparsec       as M
import qualified Text.Megaparsec.Error as E

import           Parser                (BinOp (..), Expr (..), expr, varList)

data Command = Print String
             | Eval String String
             deriving Show

eval :: (Floating a) => M.Map String a -> Expr Integer -> a
eval m e = runReader (evalExpr e) m

operation :: (Floating a) => BinOp -> (a -> a -> a)
operation Add = (+)
operation Sub = (-)
operation Mul = (*)
operation Div = (/)
operation Pow = (**)

data VarNotDefined = VarNotDefined String
    deriving (Show, Typeable)

instance Exception VarNotDefined

evalExpr :: (Floating a) => Expr Integer -> Reader (M.Map String a) a
evalExpr (Expr op e1 e2) = evalExpr e1 >>= (\x -> evalExpr e2 >>= \y -> reader $ \_ -> operation op x y)
evalExpr (Lit e) = reader $ \_ -> fromInteger e
evalExpr (Neg e) = evalExpr e >>=  \x -> reader $ \_ -> negate x
evalExpr (Var e) = reader $ \env -> fromMaybe (throw $ VarNotDefined e) (M.lookup e env)

main :: IO ()
main = do
  c <- O.execParser $ O.info (O.helper O.<*> parseOptions) O.fullDesc
  case c of
    Print e -> case M.parse expr "" e of
                 Left err  -> putStrLn $ E.parseErrorPretty err
                 Right ans -> print ans
    Eval e v -> case eval <$> M.fromList . map (second fromInteger) <$> M.parse varList "" v <*> (M.parse expr "" e) of
                  Left err  -> putStrLn $ E.parseErrorPretty err
                  Right val -> print val
                `catch` (\(VarNotDefined x) -> putStrLn $ "Evaluation error: variable " ++ x ++ " is not defined")

withInfo :: O.Parser a -> String -> O.ParserInfo a
withInfo opts desc = O.info (O.helper <*> opts) $ O.progDesc desc

parseEval :: O.Parser Command
parseEval = Eval
           <$> O.strOption
               ( O.long "expr"
               O.<> O.metavar "EXPR"
               O.<> O.help "Expression for evaluation")
           <*> O.strOption
               ( O.long "var"
               O.<> O.value ""
               O.<> O.metavar "VAR"
               O.<> O.help "Variables to be used while evaluating")

parsePrint :: O.Parser Command
parsePrint = Print
           <$> O.strOption
               ( O.long "expr"
               O.<> O.metavar "EXPR"
               O.<> O.help "Expression for evaluation")

parseOptions :: O.Parser Command
parseOptions = O.subparser $
    O.command "eval"  (parseEval `withInfo` "Eval expression with possible set of variables") O.<>
    O.command "print-ast"  (parsePrint `withInfo` "Print AST for expression")
