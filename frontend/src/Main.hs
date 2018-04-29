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
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Tree hiding (drawTree)

import Reflex.Dom (mainWidgetWithCss)
import Reflex.Dom.SemanticUI hiding (mainWidgetWithCss)

import Common ((<<$>>))
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

-- | Text input that triggers on hitting enter.
quickEntry :: MonadWidget t m => Text -> m (Event t Text)
quickEntry placeholder = input def $ do
  textInp <- textInput (def & textInputConfig_placeholder |~ placeholder)
  let txt = _textInput_value textInp
  let send = () <$ ffilter (== 13) (_textInput_keypress textInp)
  return $ tagPromptlyDyn txt send

drawTree :: MonadWidget t m => MotifResponse -> m (Event t MotifAction)
drawTree (motifEnv, t) = segment def $ do
  message def $ text $ "MotifEnv: " <> tshow motifEnv
  addToInbox <- quickEntry "Add to Inbox..."
  treeAction <- segment def $ go [t]
  return $ leftmost [
      treeAction
    , MotifActionAddToInbox <$> addToInbox
    ]
  where
    actionButton :: UI t m => Active t Text -> Color -> m (Event t ())
    actionButton iconName color = button (def & buttonConfig_icon |~ True) $
      icon iconName (def & iconConfig_color |?~ color)
    drawNode :: UI t m => MotifNode -> m (Event t MotifAction) -> m (Event t MotifAction)
    drawNode node extra = listItem (def & listItemConfig_preContent ?~ icon "file" def) $ do
      title <- fmap fst $ listHeader' $ do
        text $ getText node
        forM_ (getContext node) $ label def . text . tshow
      deleteNode <- listDescription $
        actionButton "delete" Red
      extraEvt <- extra
      return $ leftmost
        [ MotifActionDelete (_motifNodeID node) <$ deleteNode
        , MotifActionSetOpen (_motifNodeID node) (not $ _motifNodeOpen node) <$  domEvent Click title
        -- TODO: This should open editor.
        , MotifActionSetOpen (_motifNodeID node) (not $ _motifNodeOpen node) <$  domEvent Dblclick title
        , extraEvt]
    go :: UI t m => [Tree MotifNode] -> m (Event t MotifAction)
    go = \case
      [] -> return never
      xs ->
        fmap leftmost $ list def $ forM xs $ \(Node node children) ->
          drawNode node $ case children of
            [] -> return never
            _ -> if _motifNodeOpen node
              then go children
              else do
                el "div" $ text "<collapsed>"
                return never
