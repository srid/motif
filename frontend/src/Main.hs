{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Monad (forM, forM_)
import Data.Default (Default (def))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Tree hiding (drawTree)

import Reflex.Dom (mainWidgetWithCss)
import Reflex.Dom.SemanticUI hiding (mainWidgetWithCss)

import Common ((<<$), (<<$>>))
import Common.Types

import qualified Frontend.Client as Client
import Frontend.Ouroboros

main :: IO ()
main = mainWidgetWithCss css app
  where
    css = "@import url(https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.1/semantic.min.css);"

app :: forall t m. MonadWidget t m => m ()
app = ouroboros
  (container def . either showError drawTree)
  ((Client.unzipResult <<$>>) . Client.sendAction)
  MotifActionGet
  (text "Loading...")

showError :: UI t m => Text -> m (Event t MotifAction)
showError err = do
  text $ "Error: " <> err
  return never

-- 1. Expand collapse, and save 'tree state'
drawTree :: MonadWidget t m => (FilePath, Motif) -> m (Event t MotifAction)
drawTree (dbPath, t) = segment def $ do
  message def $ text $ "Using DB: " <> tshow dbPath
  txt <- _textInput_value <$> textInput def
  addToInbox <- (MotifActionAddToInbox <$>) . tagPromptlyDyn txt <$> do
    button (def & buttonConfig_type .~ SubmitButton) $ text "Add"
  treeAction <- segment def $ go [unMomentTree $ _motifTree t]
  return $ leftmost [addToInbox, treeAction]
  where
    go :: UI t m => [MotifTree Moment] -> m (Event t MotifAction)
    go = \case
      [] -> do
        blank
        return never
      xs ->
        fmap leftmost $ list def $ forM xs $ \case
          Node v@(id', _, _) [] -> listItem (def & listItemConfig_preContent ?~ icon "file" def) $ do
            listHeader $ do
              text $ getText v
              forM_ (getContext v) $ label def . text . tshow
            listDescription $ text "Leaf content"
            MotifActionDelete id' <<$ do
              button (def & buttonConfig_type .~ SubmitButton) $ text "DEL"
          Node v@(id', st, _) xs' -> listItem (def & listItemConfig_preContent ?~ icon "folder" def) $ do
            evt <- listHeader $ do
              text $ getText v
              MotifActionSetNodeState id' (toggleIt st) <<$ button def (text "Collapse")
            listDescription $ text $ "Node w/ " <> tshow (length xs') <> " children"
            childEvt <- if _nodeStateOpen st
              then go xs'
              else do
                text "<collapsed>"
                return never
            return $ leftmost [evt, childEvt]
    toggleIt st = st { _nodeStateOpen = not $ _nodeStateOpen st }
