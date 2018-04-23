{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Backend.Server (runServer, Env(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Maybe (catMaybes)
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
import Network.Wai.Middleware.Static (static)

import Common.Types

import qualified Backend.Database as Database

-- TODO: Pass basic config to frontend. Mainly to display Db path.
data Env = Env
  { _envPort :: Int
  , _envDbPath :: FilePath
  , _envAcid :: Acid.AcidState Motif
  }

type AppM = ReaderT Env Handler

app :: Env -> Application
app e = serve (Proxy @MotifAPI) $
  hoistServer (Proxy @MotifAPI) (`runReaderT` e) motifServer

motifServer :: ServerT MotifAPI AppM
motifServer = sendAction
  where
    sendAction :: MotifAction -> AppM (Either Text Motif)
    sendAction = \case
      MotifActionGet -> do
        db <- reader _envAcid
        liftIO $ Right <$> Database.get db
      MotifActionAddToInbox s -> do
        db <- reader _envAcid
        d <- liftIO $ Database.get db
        uuid <- liftIO UUID.nextRandom
        let node = Node (uuid, def :: NodeState, MomentInbox (Content s)) []
        let motif' = Motif $ MomentTree $ addNode node $ unMomentTree $ _motifTree d
        liftIO $ Database.put db motif'
        return $ Right motif'
      MotifActionDelete id' -> do
        db <- reader _envAcid
        d <- liftIO $ Database.get db
        let motif' = Motif $ MomentTree $ deleteNode id' $ unMomentTree $ _motifTree d
        liftIO $ Database.put db motif'
        return $ Right motif'
      MotifActionSetNodeState id' state -> do
        db <- reader _envAcid
        d <- liftIO $ Database.get db
        let motif' = Motif $ MomentTree $ setState id' state $ unMomentTree $ _motifTree d
        liftIO $ Database.put db motif'
        return $ Right motif'

-- TODO: Replace these set of functions using Tree functor map

-- | Add a node to the top-level level1 node.
addNode :: Tree a -> Tree a -> Tree a
addNode n (Node v xs) = Node v $ n : xs

deleteNode :: UUID -> Tree (UUID, a, b) -> Tree (UUID, a, b)
deleteNode id' (Node v xs) = Node v $ deleteNode' id' xs

deleteNode' :: UUID -> Forest (UUID, a, b) -> Forest (UUID, a, b)
deleteNode' id' ts = catMaybes $ fmap go ts
  where
    go (Node v@(id'', _, _) xs) = if id' == id''
      then Nothing
      else Just $ Node v $ catMaybes $ fmap go xs

setState' :: UUID -> NodeState -> MotifTree a -> MotifTree a
setState' id' state = fmap f
  where
    f v@(id'', _oldState, x)
      | id' == id'' = (id', state, x)
      | otherwise = v

setState :: UUID -> NodeState -> MotifTree Moment -> MotifTree Moment
setState id' state =
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
  liftIO $ run (_envPort e) $ corsWithContentType $ logStdoutDev $ static $ app e
  where
    -- | Allow Content-Type header with values other then allowed by simpleCors.
    corsWithContentType = cors (const $ Just policy)
      where
        policy = simpleCorsResourcePolicy
          { corsRequestHeaders = ["Content-Type"] }
