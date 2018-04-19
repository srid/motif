{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
-- The ouroboros is an ancient symbol depicting a serpent or dragon eating its own tail.
module Frontend.Ouroboros
  ( ouroboros
  ) where

import Reflex.Dom.SemanticUI

type View model action =
  forall t m. UI t m => model -> m (Event t action)
type Update model action =
  forall t m. MonadWidget t m => Event t action -> m (Event t model)

-- NOTE: Unlike Elm's TEA our update function is intended to actually perform
-- the update on the server. We might have to revisit this when needing
-- exclusively client-side updates.
ouroboros
  :: MonadWidget t m
  => action
  -> View model action
  -> Update model action
  -> m ()
ouroboros action0 view update = do
  result <- (update . (action0 <$)) =<< getPostBuild
  widgetHold_ (text "Loading...") $ ffor result $ \r -> do
    rec modelDyn <- holdDyn r modelEvent
        modelEvent <- switchModel view update modelDyn
    return ()

switchModel
  :: MonadWidget t m
  => View model action
  -> Update model action
  -> Dynamic t model
  -> m (Event t model)
switchModel view update modelDyn = do
  e' <- dyn $ view <$> modelDyn
  e <- switch <$> hold never e'
  update e
