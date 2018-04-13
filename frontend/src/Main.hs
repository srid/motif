{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom

main :: IO ()
main = mainWidget $ text "hi - this is motif"
