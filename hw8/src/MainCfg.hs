{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Exception  as E
import qualified Control.Monad.Catch as MC
import           Control.Monad      (forever)
import qualified Data.IORef         as IORef
import           Data.List          (null)
import qualified Data.Map           as M
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified System.Environment as SE
import           System.Exit        (exitFailure, exitSuccess)
import qualified Control.Monad.Trans.Either as TE
import qualified Control.Monad.Trans as Trans

parseCfg :: T.Text -> M.Map T.Text T.Text
parseCfg s = parseCfgList M.empty $ T.split (=='\n') s
  where parseCfgList m [] = m
        parseCfgList m (x:xs) = parseCfgList newM xs
          where newM = case T.breakOn (T.singleton '=') x of
                  (_, "") -> m
                  (_, "=") -> m
                  (prop, val) -> M.insert prop (T.tail val) m

toStringCfg :: M.Map T.Text T.Text -> T.Text
toStringCfg m = T.intercalate "\n" $ map (\(a, b) -> a `T.append` "=" `T.append` b) $ M.toList m

data Command = Abort | Write | Update T.Text T.Text | Add T.Text T.Text

failure :: T.Text -> TE.EitherT T.Text IO a
failure t = TE.EitherT $ return $ Left t

parseCommand :: IORef.IORef (M.Map T.Text T.Text) -> TE.EitherT T.Text IO Command
parseCommand config = do
  res <- Trans.lift T.getLine
  cfg <- Trans.lift $ IORef.readIORef config
  case T.head res of
    'A' -> return Abort
    'W' -> return Write
    _ -> case T.take 2 res of
           "B " -> case T.drop 2 res of
                     "" -> failure "Empty property can't be updated"
                     p -> case M.lookup p cfg of
                       Just val -> return $ Update p val
                       Nothing -> failure "Property doesn't exist and can't be updated"
           _ -> case T.breakOn (T.singleton '=') res of
                  (_, "") -> failure "Command is not valid"
                  (_, "=") -> failure "Property value is empty"
                  (prop, val) -> case M.lookup prop cfg of
                                   Nothing -> return $ Add prop $ T.tail val
                                   Just _ -> failure "Property already exists, use modification command"

processCommand :: String -> IORef.IORef (M.Map T.Text T.Text) -> Command -> TE.EitherT T.Text IO ()
processCommand _ _ Abort = do
  Trans.lift $ T.putStrLn "Stopping cfg-creator"
  Trans.lift exitFailure
processCommand file config Write = do
  cfg <- Trans.lift $ IORef.readIORef config
  Trans.lift $ T.putStrLn $ "Writing config to file " `T.append` T.pack file
  MC.catch (Trans.lift $ T.writeFile file $ toStringCfg cfg)
      (\e -> failure $ T.pack $ "Warning: " ++ show (e :: E.IOException))
  Trans.lift exitSuccess
processCommand _ config (Update prop oldVal) = do
  cfg <- Trans.lift $ IORef.readIORef config
  Trans.lift $ T.putStrLn $ "Enter new value for " `T.append` prop `T.append` ". Previous value: " `T.append` oldVal
  val <- Trans.lift T.getLine
  Trans.lift $ IORef.writeIORef config (M.insert prop val cfg)
processCommand _ config (Add prop val) = do
  cfg <- Trans.lift $ IORef.readIORef config
  Trans.lift $ IORef.writeIORef config (M.insert prop val cfg)

main :: IO ()
main = do
  args <- SE.getArgs
  if null args then do
    T.putStrLn "File name not specified"
    return ()
  else do
    let file = head args
    cfgText <- E.catch (T.readFile file) ((\_ -> return  "") :: E.IOException -> IO T.Text)
    config <- IORef.newIORef $ parseCfg cfgText
    forever $ do
      T.putStrLn "Input property and value:"
      res <- TE.runEitherT $ parseCommand config >>= processCommand file config
      case res of
        Left err -> T.putStrLn err
        Right _ -> return ()
