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
module Frontend.Client
  ( getMotif
  , sendAction
  , withResult
  , unzipResult
  ) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)

import Reflex.Dom.SemanticUI
import Servant.API
import Servant.Reflex

import Common

-- TODO: Start using ReaderT to specify the jsaddle-warp URL.
serverUrl :: BaseUrl
serverUrl = BaseFullUrl Http "localhost" 3001 "/"

type GetMotif t m = Event t () -> m (Event t (ReqResult () (Either Text Motif)))
type SendAction t m = Dynamic t (Either Text MotifAction) -> Event t () -> m (Event t (ReqResult () (Either Text Motif)))

motifClient :: forall t m. MonadWidget t m => GetMotif t m :<|> SendAction t m
motifClient = client (Proxy :: Proxy MotifAPI) (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn serverUrl)

getMotif :: MonadWidget t m => GetMotif t m
sendAction' :: MonadWidget t m => SendAction t m
getMotif :<|> sendAction' = motifClient

sendAction
  :: forall t m. MonadWidget t m
  => Event t MotifAction -> m (Event t (ReqResult () (Either Text Motif)))
sendAction = patchServantClientF sendAction'

-- | Helper to get rid of the Dynamic in servant-reflex functions (of one argument only)
patchServantClientF
  :: MonadWidget t m
  => (Dynamic t (Either Text a) -> Event t () -> m (Event t r))
  -> Event t a
  -> m (Event t r)
patchServantClientF f evt = do
  d <- holdDyn (Left "No value yet") $ Right <$> evt
  f d $ () <$ evt

withResult :: UI t m => ReqResult tag (Either Text a) -> (Text -> m ()) -> (a -> m ()) -> m ()
withResult r ef sf = either ef sf $ unzipResult r

unzipResult :: ReqResult tag (Either Text a) -> Either Text a
unzipResult r = case r of
  ResponseFailure _ s _ -> Left s
  RequestFailure _ s -> Left s
  ResponseSuccess _ (Left s) _ -> Left s
  ResponseSuccess _ (Right v) _ -> Right v
