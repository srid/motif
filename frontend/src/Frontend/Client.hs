{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Frontend.Client
  ( sendAction
  , unzipResult
  ) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)

import Reflex.Dom.SemanticUI
import Servant.Reflex

import Common.Types (Motif, MotifAPI, MotifAction, MotifEnv)

-- TODO: Start using ReaderT to specify the jsaddle-warp URL.
#if defined(ghcjs_HOST_OS)
serverUrl :: BaseUrl
serverUrl = BasePath "/"
#else
serverUrl :: BaseUrl
serverUrl = BaseFullUrl Http "localhost" 3001 "/"
#endif

type SendAction t m = Dynamic t (Either Text MotifAction) -> Event t () -> m (Event t (ReqResult () (Either Text (MotifEnv, Motif))))

motifClient :: forall t m. MonadWidget t m => SendAction t m
motifClient = client (Proxy @MotifAPI) (Proxy @m) (Proxy @()) (constDyn serverUrl)

sendAction' :: MonadWidget t m => SendAction t m
sendAction' = motifClient

sendAction
  :: forall t m. MonadWidget t m
  => Event t MotifAction -> m (Event t (ReqResult () (Either Text (MotifEnv, Motif))))
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

-- | Flatten the two level errors into one.
-- TODO: At one point we want to treat these errors differently.
unzipResult :: ReqResult tag (Either Text a) -> Either Text a
unzipResult r = case r of
  ResponseFailure _ s _ -> Left s
  RequestFailure _ s -> Left s
  ResponseSuccess _ (Left s) _ -> Left s
  ResponseSuccess _ (Right v) _ -> Right v
