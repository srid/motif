module Main where

import Control.Exception (bracket)
import Control.Monad.Reader

import Data.Acid (createCheckpoint)

import Backend.Database (closeDb, openDb)
import Backend.Server

main :: IO ()
main =
  bracket openDb closeDb $ \db -> do
    createCheckpoint db
    putStrLn "Created checkpoint"
    runServer `runReaderT` Env db
