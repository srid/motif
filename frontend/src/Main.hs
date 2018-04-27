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
  (icon "spinner" $ def & iconConfig_loading |~ True)

showError :: UI t m => Text -> m (Event t MotifAction)
showError err = message (def & messageConfig_type |?~ MessageType Negative) $ do
  text $ "Error: " <> err
  return never

-- TODO: refactor
drawTree :: MonadWidget t m => MotifResponse -> m (Event t MotifAction)
drawTree (motifEnv, t) = segment def $ do
  message def $ text $ "MotifEnv: " <> tshow motifEnv
  addToInbox <- input (def & inputConfig_action |?~ RightAction) $ do
    txt <- _textInput_value <$> textInput (def & textInputConfig_placeholder |~ "Add to Inbox..")
    fmap MotifActionAddToInbox . tagPromptlyDyn txt <$> do
      button def $ text "Add"
  treeAction <- segment def $ go [t]
  return $ leftmost [addToInbox, treeAction]
  where
    go :: UI t m => [MotifTree Moment] -> m (Event t MotifAction)
    go = \case
      [] -> return never
      xs ->
        fmap leftmost $ list def $ forM xs $ \case
          Node v@(id', _, _) [] -> listItem (def & listItemConfig_preContent ?~ icon "file" def) $ do
            listHeader $ do
              text $ getText v
              forM_ (getContext v) $ label def . text . tshow
            listDescription $ text "Leaf content"
            MotifActionDelete id' <<$ do
              button (def & buttonConfig_type .~ SubmitButton & buttonConfig_icon |~ True) $
                icon "delete" (def & iconConfig_color |?~ Red)
          Node v@(id', st, _) xs' -> listItem (def & listItemConfig_preContent ?~ icon "folder" def) $ do
            evt <- fmap (domEvent Click . fst) $ listHeader' $
              -- TODO: hover over mouse cursor
              text $ getText v
            listDescription $ text $ "Node w/ " <> tshow (length xs') <> " children"
            childEvt <- if _nodeStateOpen st
              then go xs'
              else do
                text "<collapsed>"
                return never
            return $ leftmost [
                MotifActionSetNodeState id' (toggleIt st) <$ evt
              , childEvt
              ]
    toggleIt st = st { _nodeStateOpen = not $ _nodeStateOpen st }
