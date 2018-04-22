{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Client where

import Data.Proxy (Proxy (..))
import Data.Text (Text)

-- import qualified JSDOM.Generated.XMLHttpRequest as XHR
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import Reflex.Dom.SemanticUI
import Reflex.Servant
-- import Reflex.Servant.Internal.GenericClientM
import Servant.Client.Core
import Servant.Client.JSaddle (fixUpXhr, mkClientEnv, runClientM)

import Common (MotifAPI)
import Common.Types (Motif, MotifAction)

-- TODO: Start using ReaderT to specify the jsaddle-warp URL.
serverUrl :: BaseUrl
serverUrl = BaseUrl Http "localhost" 3001 ""

motifClient
  :: MonadWidget t m
  => ReflexClient (Config (InstantiatedEndpointConfig t m) Tuple) MotifAPI
motifClient = reflexClient (basicConfig myRunner) (Proxy @MotifAPI)

myRunner :: MonadJSM m => ServantClientRunner () m
myRunner cfg (GenericClientM m) = servantClientRunner cfg m

servantClientRunner
  :: MonadJSM m
  => ()
  -> GenericClientM a
  -> m (Either ServantError a)
servantClientRunner _ m = liftJSM $ do
  -- TODO: Figure out where `getXSRF` is coming from, and undo the commenting out below.
  let clientEnv = (mkClientEnv serverUrl)
        { fixUpXhr = \_xhr -> return () -- do
            -- XHR.setRequestHeader xhr ("X-XSRF-TOKEN" :: Text) =<< getXSRF
        }
  runGenericClientM m `runClientM` clientEnv

sendAction
  :: MonadWidget t m
  => Event t MotifAction
  -> m (Event t (Either ServantError (Either Text Motif)))
sendAction = motifClient
