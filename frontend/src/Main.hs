{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T

import Reflex.Dom
import Servant.Reflex

import Common

-- TODO: Start using ReaderT to specify the jsaddle-warp URL.
serverUrl :: BaseUrl
serverUrl = BaseFullUrl Http "localhost" 3001 "/"

main :: IO ()
main = mainWidget $ do
  result <- getPostBuild >>= motifClient
  widgetHold_ (text "Loading...") $ ffor result $ \r -> case reqFailure r of
    Just e -> text $ "Error: " <> e
    Nothing -> el "tt" $ text $ "Response: " <> T.pack (show $ reqSuccess r)

motifClient
  :: forall t m. MonadWidget t m
  => Event t ()
  -> m (Event t (ReqResult () (Either Text Motif)))
motifClient = client
  (Proxy :: Proxy MotifAPI)
  (Proxy :: Proxy m)
  (Proxy :: Proxy ())
  (constDyn serverUrl)

