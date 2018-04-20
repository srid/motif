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
import Data.Text (Text)
import Data.Tree

import qualified Data.Acid as Acid
import Data.Default (def)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID

import Servant
import Servant.Server (hoistServer)

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Common

import Backend.Database (Database)
import qualified Backend.Database as Database

newtype Env = Env
  { acid :: Acid.AcidState Database
  }

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
      MotifActionGet -> do
        db <- reader acid
        liftIO $ Right <$> Database.get db
      MotifAddToInbox s -> do
        db <- reader acid
        d <- liftIO $ Database.get db
        uuid <- liftIO UUID.nextRandom
        let node = Node (uuid, def :: NodeState, MomentInbox (Content s)) []
        let motif' = Motif "changed" $ MomentTree $ addNode node $ unMomentTree $ _motifMomentTree d
        liftIO $ Database.put db motif'
        return $ Right motif'
      MotifActionSetNodeState id' state -> do
        db <- reader acid
        d <- liftIO $ Database.get db
        let motif' = Motif "changed" $ MomentTree $ setState id' state $ unMomentTree $ _motifMomentTree d
        liftIO $ Database.put db motif'
        return $ Right motif'

addNode :: MotifTree Moment -> MotifTree Moment -> MotifTree Moment
addNode n t = n { subForest = [t] }

setState :: UUID -> NodeState -> MotifTree Moment -> MotifTree Moment
setState id' state =
  -- TODO: replace with map
  \case
    Node v@(id'', _oldState, x) c ->
      if id'' == id'
        then Node (id', state, x) c
        else Node v $ setState id' state <$> c

runServer
  :: (Functor m, MonadReader Env m, MonadIO m)
  => m ()
runServer = do
  e <- ask
  liftIO $ putStrLn "Running server at http://localhost:3001/"
  liftIO $ run 3001 $ corsWithContentType $ logStdoutDev $ app e
  where
    -- | Allow Content-Type header with values other then allowed by simpleCors.
    corsWithContentType = cors (const $ Just policy)
      where
        policy = simpleCorsResourcePolicy
          { corsRequestHeaders = ["Content-Type"] }
