module Main where

import Control.Monad.Reader

import Backend.Server

main :: IO ()
main = runServer `runReaderT` Env
