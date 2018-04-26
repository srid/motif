module Main where

import Control.Exception (bracket)
import Control.Monad.Reader
import Data.Monoid ((<>))
import System.Environment (getArgs)

import Data.Acid (createCheckpoint)
import System.Directory (makeAbsolute)

import Common.Types (MotifEnv (MotifEnv))

import Backend.Database (closeDb, openDb)
import Backend.Server

main :: IO ()
main = do
  [port', dbPath'] <- take 2 <$> getArgs
  dbPath <- makeAbsolute dbPath'
  let motifEnv = MotifEnv (read port') dbPath
  bracket (openDb dbPath) closeDb $ \db -> do
    createCheckpoint db
    putStrLn $ "Created checkpoint for db " <> dbPath
    runServer `runReaderT` Config motifEnv db
