module Main where

import Control.Monad.Reader

import Backend.Database (openDb)
import Backend.Server

main :: IO ()
main = do
  db <- openDb
  runServer `runReaderT` Env db
