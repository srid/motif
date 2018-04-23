module Main where

import Control.Exception (bracket)
import Control.Monad.Reader
import Data.Monoid ((<>))
import System.Environment (getArgs)

import Data.Acid (createCheckpoint)
import System.Directory (makeAbsolute)

import Backend.Database (closeDb, openDb)
import Backend.Server

main :: IO ()
main = do
  [port', dbPath'] <- take 2 <$> getArgs
  dbPath <- makeAbsolute dbPath'
  bracket (openDb dbPath) closeDb $ \db -> do
    createCheckpoint db
    putStrLn $ "Created checkpoint for db " <> dbPath
    runServer `runReaderT` Env (read port') dbPath db
