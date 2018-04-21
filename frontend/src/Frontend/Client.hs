{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
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
module Frontend.Client where

import Data.Proxy (Proxy (..))
import Data.Text (Text)

import Reflex.Dom.SemanticUI
import Reflex.Servant
import Servant.API
import Servant.Client.Core

import Common (MotifAPI)
import Common.Types (Motif, MotifAction)

import JSaddleXhrClient

-- TODO: Start using ReaderT to specify the jsaddle-warp URL.
serverUrl :: BaseUrl
serverUrl = BaseUrl Http "localhost" 3001 "/"

-- type SendAction t m = Dynamic t (Either Text MotifAction) -> Event t () -> m (Event t (ReqResult () (Either Text Motif)))

motifClient :: MonadWidget t m => ReflexClient (Config (InstantiatedEndpointConfig t m) Tuple) MotifAPI
motifClient = reflexClient (basicConfig myRunner) (Proxy @MotifAPI)

myRunner :: ServantClientRunner () m
myRunner cfg (GenericClientM m) = myRunServantClient cfg m

myRunServantClient :: () -> (RunClient m' => m' a) -> m (Either ServantError a)
myRunServantClient cfg m = runClientM m
  where
    cEnv = mkClientEnv serverUrl

sendAction :: MonadWidget t m => Event t MotifAction -> m (Event t (Either ServantError (Either Text Motif)))
sendAction = motifClient

-- sendAction' :: MonadWidget t m => SendAction t m
-- sendAction' = motifClient

-- sendAction
--   :: forall t m. MonadWidget t m
--   => Event t MotifAction -> m (Event t (ReqResult () (Either Text Motif)))
-- sendAction = patchServantClientF sendAction'

-- -- | Helper to get rid of the Dynamic in servant-reflex functions (of one argument only)
-- patchServantClientF
--   :: MonadWidget t m
--   => (Dynamic t (Either Text a) -> Event t () -> m (Event t r))
--   -> Event t a
--   -> m (Event t r)
-- patchServantClientF f evt = do
--   d <- holdDyn (Left "No value yet") $ Right <$> evt
--   f d $ () <$ evt

-- -- | Flatten the two level errors into one.
-- -- TODO: At one point we want to treat these errors differently.
-- unzipResult :: ReqResult tag (Either Text a) -> Either Text a
-- unzipResult r = case r of
--   ResponseFailure _ s _ -> Left s
--   RequestFailure _ s -> Left s
--   ResponseSuccess _ (Left s) _ -> Left s
--   ResponseSuccess _ (Right v) _ -> Right v
