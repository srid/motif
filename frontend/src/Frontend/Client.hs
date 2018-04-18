{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Frontend.Client where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.UUID (UUID)

import Reflex.Dom
import Servant.API
import Servant.Reflex

import Common

-- TODO: Start using ReaderT to specify the jsaddle-warp URL.
serverUrl :: BaseUrl
serverUrl = BaseFullUrl Http "localhost" 3001 "/"

motifClient
  :: forall t m. MonadWidget t m
  => (Event t () -> m (Event t (ReqResult () (Either Text Motif))))
      :<|> (Dynamic t (Either Text (UUID, NodeState)) -> Event t () -> m (Event t (ReqResult () (Either Text Motif))))
motifClient = client (Proxy :: Proxy MotifAPI) (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn serverUrl)

getMotif :: forall t m. MonadWidget t m => Event t () -> m (Event t (ReqResult () (Either Text Motif)))
editMotif :: forall t m. MonadWidget t m => Dynamic t (Either Text (UUID, NodeState)) -> Event t () -> m (Event t (ReqResult () (Either Text Motif)))
getMotif :<|> editMotif = motifClient
