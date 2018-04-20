module Main where

import Control.Exception (bracket)
import Control.Monad.Reader

import Backend.Database (closeDb, openDb)
import Backend.Server

main :: IO ()
main =
  bracket openDb closeDb $ \db ->
    runServer `runReaderT` Env db
