{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
-- The ouroboros is an ancient symbol depicting a serpent or dragon eating its own tail.
module Frontend.Ouroboros
  ( ouroboros
  ) where

import Reflex.Dom.SemanticUI

type View t m model action =
  model -> m (Event t action)
type Update t m model action =
  Event t action -> m (Event t model)

-- NOTE: Unlike Elm's TEA our update function is intended to actually perform
-- the update on the server. We might have to revisit this when needing
-- exclusively client-side updates.
ouroboros
  :: MonadWidget t m
  => View t m model action
  -> Update t m model action
  -> action
  -> m ()
  -> m ()
ouroboros view update action0 widget0 = do
  result <- update . (action0 <$) =<< getPostBuild
  widgetHold_ widget0 $ ffor result $ \model0 -> do
    rec modelDyn <- holdDyn model0 modelEvent
        modelEvent <- switchModel view update modelDyn
    return ()

switchModel
  :: MonadWidget t m
  => View t m model action
  -> Update t m model action
  -> Dynamic t model
  -> m (Event t model)
switchModel view update modelDyn = do
  e' <- dyn $ view <$> modelDyn
  e <- switch <$> hold never e'
  update e
