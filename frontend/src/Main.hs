{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Monad (forM, forM_)
import Data.Default (Default (def))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Tree hiding (drawTree)
import Data.UUID (UUID)

import Reflex.Dom (mainWidgetWithCss)
import Reflex.Dom.SemanticUI hiding (mainWidgetWithCss)
import Servant.Reflex

import Common

import qualified Frontend.Client as Client

data MotifAction
  = MotifActionSetNodeState UUID NodeState
  deriving (Eq, Show, Ord)

-- TODO: Start using ReaderT to specify the jsaddle-warp URL.
serverUrl :: BaseUrl
serverUrl = BaseFullUrl Http "localhost" 3001 "/"

main :: IO ()
main = mainWidgetWithCss css app
  where
    css = "@import url(https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/semantic.min.css);"

app :: forall t m. MonadWidget t m => m ()
app = do
  pb <- getPostBuild
  result <- Client.getMotif pb
  widgetHold_ (text "Loading...") $ ffor result $ \r ->
    container def $ withMotifResult r (text . ("Error: " <>)) $ \t -> do
      rec treeDyn <- holdDyn t updatedTree
          updatedTree <- renderAndUpdate drawTree treeDyn
      return ()
  where
    applyAction :: MonadWidget t m => Event t MotifAction -> m (Event t (ReqResult () (Either Text Motif)))
    applyAction evt = do
      let setNodeState = fforMaybe evt $ \case
            MotifActionSetNodeState id' state -> Just (id', state)
      requestingClient Client.editMotif setNodeState
    renderAndUpdate :: UI t m => (Motif -> m (Event t MotifAction)) -> Dynamic t Motif -> m (Event t Motif)
    renderAndUpdate f d = do
      e' <- dyn $ ffor d f
      e <- switch <$> hold never e'
      filterRight <$> unzipMotifResult <<$>> applyAction e

requestingClient
  :: MonadWidget t m
  => (Dynamic t (Either Text a) -> Event t () -> m (Event t r))
  -> Event t a
  -> m (Event t r)
requestingClient f evt = do
  d <- holdDyn (Left "No value yet") $ Right <$> evt
  f d $ () <$ evt

-- TODO: Nice and cool tree UI
-- 1. Expand collapse, and save 'tree state'
drawTree :: UI t m => Motif -> m (Event t MotifAction)
drawTree t = go [unMomentTree $ _motifMomentTree t]
  where
    go :: UI t m => [MotifTree Moment] -> m (Event t MotifAction)
    go = \case
      [] -> do
        blank
        return never
      xs -> do
        evts <- list def $ forM xs $ \case
          Node v [] -> listItem (def & listItemConfig_preContent ?~ icon "file" def) $ do
            listHeader $ do
              text $ getText v
              forM_ (getContext v) $ label def . text . tshow
            listDescription $ text "Leaf content"
            return never
          Node v@(id', st, _) xs' -> listItem (def & listItemConfig_preContent ?~ icon "folder" def) $ do
            -- TODO: Show children only if `_state[open]`.
            evt <- listHeader $ do
              text $ getText v
              MotifActionSetNodeState id' (toggleIt st) <<$ button def (text "Collapse")
            listDescription $ text $ "Node w/ " <> tshow (length xs') <> " children"
            childEvt <- if _nodeStateOpen st then go xs' else return never
            return $ leftmost [evt, childEvt]
        return $ leftmost evts
    toggleIt st = st { _nodeStateOpen = not $ _nodeStateOpen st }

moment :: (UI t m, IsMoment a) => a -> m ()
moment v = do
  divClass "text" $ text $ getText v
  forM_ (getContext v) $ label def . text . tshow

withMotifResult :: UI t m => ReqResult tag (Either Text a) -> (Text -> m ()) -> (a -> m ()) -> m ()
withMotifResult r ef sf = either ef sf $ unzipMotifResult r

unzipMotifResult :: ReqResult tag (Either Text a) -> Either Text a
unzipMotifResult r = case r of
  ResponseFailure _ s _ -> Left s
  RequestFailure _ s -> Left s
  ResponseSuccess _ (Left s) _ -> Left s
  ResponseSuccess _ (Right v) _ -> Right v

