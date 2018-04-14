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

import Reflex.Dom (mainWidgetWithCss)
import Reflex.Dom.SemanticUI hiding (mainWidgetWithCss)
import Servant.Reflex

import Common

-- TODO: Start using ReaderT to specify the jsaddle-warp URL.
serverUrl :: BaseUrl
serverUrl = BaseFullUrl Http "localhost" 3001 "/"

main :: IO ()
main = mainWidgetWithCss css $ do
  result <- getPostBuild >>= motifClient
  widgetHold_ (text "Loading...") $ ffor result $ \r ->
    container def $ case reqFailure r of
      Just e -> paragraph $ text $ "Error: " <> e
      Nothing -> paragraph $ el "tt" $ text $ "Response: " <> T.pack (show $ reqSuccess r)
  where
    css =
      "@import url(https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/semantic.min.css);\
      \.asis { font-family: monospace; white-space: pre-wrap; }";

motifClient
  :: forall t m. MonadWidget t m
  => Event t ()
  -> m (Event t (ReqResult () (Either Text Motif)))
motifClient = client
  (Proxy :: Proxy MotifAPI)
  (Proxy :: Proxy m)
  (Proxy :: Proxy ())
  (constDyn serverUrl)
