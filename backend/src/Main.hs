module Main where

import Control.Exception (bracket)
import Control.Monad.Reader
import Data.Monoid ((<>))
import System.Environment (getArgs)

import Data.Acid (createCheckpoint)

import Backend.Database (closeDb, openDb)
import Backend.Server

main :: IO ()
main = do
  dbPath <- head <$> getArgs
  bracket (openDb dbPath) closeDb $ \db -> do
    createCheckpoint db
    putStrLn $ "Created checkpoint for db " <> dbPath
    runServer `runReaderT` Env dbPath db
