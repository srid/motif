{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Frontend.Ouroboros where

import Data.Default (Default (def))
import Data.Monoid ((<>))
import Data.Text (Text)

import Reflex.Dom.SemanticUI

-- TODO: Make this generic enough. Looking to be more Elm like...
ouroboros
  :: forall t m model action. MonadWidget t m
  => (Event t () -> m (Event t (Either Text model)))
  -> (model -> m (Event t action))
  -> (Event t action -> m (Event t (Either Text model)))
  -> m ()
ouroboros initialModel renderModel handleAction = do
  result <- initialModel =<< getPostBuild
  widgetHold_ (text "Loading...") $ ffor result $ \r ->
    container def $ render r (text . ("Error: " <>)) $ \t -> do
      rec modelDyn <- holdDyn t modelEvent
          modelEvent <- renderAndUpdate renderModel modelDyn
      return ()
  where
    render r ef sf = either ef sf r
    renderAndUpdate :: UI t m => (model -> m (Event t action)) -> Dynamic t model -> m (Event t model)
    renderAndUpdate f d = do
      e' <- dyn $ ffor d f
      e <- switch <$> hold never e'
      filterRight <$> handleAction e
