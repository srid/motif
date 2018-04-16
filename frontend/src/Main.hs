{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad (forM_)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import Data.Text (Text)

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
    container def $ withMotifResult r (text . ("Error: " <>)) $
      drawTree . unMomentTree . _motifMomentTree
  where
    css = "@import url(https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/semantic.min.css);"

-- TODO: Nice and cool tree UI
-- 1. Expand collapse, and save 'tree state'
drawTree :: (UI t m, IsMoment a) => Tree a -> m ()
drawTree = \case
  Leaf v -> paragraph $ moment v
  Node v t ->  do
    header def $ moment v
    segment def $ forM_ t drawTree

moment :: (UI t m, IsMoment a) => a -> m ()
moment v = do
  divClass "text" $ text $ getText v
  forM_ (getContext v) $ label def . text . tshow

withMotifResult :: UI t m => ReqResult tag (Either Text a) -> (Text -> m ()) -> (a -> m ()) -> m ()
withMotifResult r ef sf = case r of
  ResponseFailure _ s _ -> ef s
  RequestFailure _ s -> ef s
  ResponseSuccess _ (Left s) _ -> ef s
  ResponseSuccess _ (Right v) _ -> sf v

motifClient :: forall t m. MonadWidget t m => Event t () -> m (Event t (ReqResult () (Either Text Motif)))
motifClient = client
  (Proxy :: Proxy MotifAPI)
  (Proxy :: Proxy m)
  (Proxy :: Proxy ())
  (constDyn serverUrl)
