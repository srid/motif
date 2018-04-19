{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Backend.Server (runServer, Env(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Default (def)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Tree
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Servant
import Servant.Server (hoistServer)

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Common

import Backend.Database

data Env = Env {}
  deriving (Eq, Show, Ord)

type AppM = ReaderT Env Handler

app :: Env -> Application
app e = serve motifAPI $ hoistServer motifAPI (`runReaderT` e) motifServer

motifAPI :: Proxy MotifAPI
motifAPI = Proxy

motifServer :: ServerT MotifAPI AppM
motifServer = sendAction
  where
    sendAction :: MotifAction -> AppM (Either Text Motif)
    sendAction = \case
      MotifActionGet ->
        return $ Right realData
      MotifActionSetNodeState id' state -> do
        let d = setState id' state $ unMomentTree $ _motifMomentTree realData
        return $ Right $ Motif "changed" $ MomentTree d

setState :: UUID -> NodeState -> MotifTree Moment -> MotifTree Moment
setState id' state =
  -- TODO: replace with map
  \case
    Node v@(id'', _oldState, x) c ->
      if id'' == id'
        then Node (id', state, x) c
        else Node v $ setState id' state <$> c

-- TODO: Start persisting this in vcache.
realData :: Motif
realData = Motif "Hello"  $ MomentTree t
  where
    t = Node (u1, def, MomentJournal [] "Inbox")
      [ Node (u2, def, MomentJournal [ContextChore] "Find a new maid") []
      , Node (u3, def, MomentJournal [ContextReading] "Read warp chapter from aosabook") []
      , Node (u4, def, MomentJournal [ContextChore] "Pay tax by end of month") []
      ]
    u1 = fromJust $ UUID.fromString "a6463901-6f36-43f5-96d8-e07b695d214d"
    u2 = fromJust $ UUID.fromString "b6463901-6f36-43f5-96d8-e07b695d214d"
    u3 = fromJust $ UUID.fromString "c6463901-6f36-43f5-96d8-e07b695d214d"
    u4 = fromJust $ UUID.fromString "d6463901-6f36-43f5-96d8-e07b695d214d"

runServer
  :: (Functor m, MonadReader Env m, MonadIO m)
  => m ()
runServer = do
  e <- ask
  liftIO $ putStrLn $ "Running server at http://localhost:3001/" <> " with config: " <> show e
  liftIO $ run 3001 $ corsWithContentType $ logStdoutDev $ app e
  where
    -- | Allow Content-Type header with values other then allowed by simpleCors.
    corsWithContentType = cors (const $ Just policy)
      where
        policy = simpleCorsResourcePolicy
          { corsRequestHeaders = ["Content-Type"] }
