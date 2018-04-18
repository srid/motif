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

import Reflex.Dom.SemanticUI
import Servant.API
import Servant.Reflex

import Common

-- TODO: Start using ReaderT to specify the jsaddle-warp URL.
serverUrl :: BaseUrl
serverUrl = BaseFullUrl Http "localhost" 3001 "/"

motifClient
  :: forall t m. MonadWidget t m
  => (Event t () -> m (Event t (ReqResult () (Either Text Motif))))
      :<|> (Dynamic t (Either Text MotifAction) -> Event t () -> m (Event t (ReqResult () (Either Text Motif))))
motifClient = client (Proxy :: Proxy MotifAPI) (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn serverUrl)

getMotif :: forall t m. MonadWidget t m => Event t () -> m (Event t (ReqResult () (Either Text Motif)))
sendAction :: forall t m. MonadWidget t m => Dynamic t (Either Text MotifAction) -> Event t () -> m (Event t (ReqResult () (Either Text Motif)))
getMotif :<|> sendAction = motifClient

requestingClient
  :: MonadWidget t m
  => (Dynamic t (Either Text a) -> Event t () -> m (Event t r))
  -> Event t a
  -> m (Event t r)
requestingClient f evt = do
  d <- holdDyn (Left "No value yet") $ Right <$> evt
  f d $ () <$ evt

withMotifResult :: UI t m => ReqResult tag (Either Text a) -> (Text -> m ()) -> (a -> m ()) -> m ()
withMotifResult r ef sf = either ef sf $ unzipMotifResult r

unzipMotifResult :: ReqResult tag (Either Text a) -> Either Text a
unzipMotifResult r = case r of
  ResponseFailure _ s _ -> Left s
  RequestFailure _ s -> Left s
  ResponseSuccess _ (Left s) _ -> Left s
  ResponseSuccess _ (Right v) _ -> Right v
