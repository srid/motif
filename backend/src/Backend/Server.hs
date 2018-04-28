{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Backend.Server (runServer, Config(..)) where

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

data Config = Config
  { _configMotifEnv :: MotifEnv
  , _configDb :: Acid.AcidState Motif
  }

type AppM = ReaderT Config Handler

app :: Config -> Application
app e = serve (Proxy @MotifAPI) $
  hoistServer (Proxy @MotifAPI) (`runReaderT` e) motifServer

motifServer :: ServerT MotifAPI AppM
motifServer = sendAction
  where
    sendAction :: MotifAction -> AppM (Either Text MotifResponse)
    sendAction = \case
      MotifActionGet -> withConfig getTree
      MotifActionAddToInbox s -> editMotif $ \tree0 -> do
        uuid <- UUID.nextRandom
        let node = Node (uuid, def :: NodeState, MomentInbox (Content s)) []
        pure $ addNode node tree0
      MotifActionDelete id' -> editMotif $ \tree0 ->
        pure $ deleteNode id' tree0
      MotifActionSetNodeState id' state -> editMotif $ \tree0 ->
        pure $ setState id' state tree0
    editMotif f = withConfig $ \db -> do
      tree0 <- getTree db
      tree1 <- f tree0
      putTree db tree1
      return tree1
    withConfig f = do
      config <- ask
      -- TODO: Will need to handle errors (Left) at some point.
      liftIO $ Right . (_configMotifEnv config, ) <$> f (_configDb config)
    getTree = fmap (unMomentTree . _motifTree) . Database.get
    putTree db = Database.put db . Motif . MomentTree

-- | Add a node to the top-level level1 node.
addNode :: Tree a -> Tree a -> Tree a
addNode n (Node v xs) = Node v $ n : xs

-- TODO: Handle the case with non-empty children.
deleteNode :: UUID -> Tree (UUID, a, b) -> Tree (UUID, a, b)
deleteNode id' (Node v xs) = Node v $ deleteNode' id' xs

deleteNode' :: UUID -> Forest (UUID, a, b) -> Forest (UUID, a, b)
deleteNode' id' ts = catMaybes $ fmap go ts
  where
    go (Node v@(id'', _, _) xs) = if id' == id''
      then Nothing
      else Just $ Node v $ catMaybes $ fmap go xs

setState :: UUID -> NodeState -> MotifTree a -> MotifTree a
setState id' state = fmap f
  where
    f v@(id'', _oldState, x)
      | id' == id'' = (id', state, x)
      | otherwise = v

runServer
  :: (Functor m, MonadReader Config m, MonadIO m)
  => m ()
runServer = do
  e <- ask
  liftIO $ putStrLn "Running server at http://localhost:3001/"
  liftIO $ run (_motifEnvPort $ _configMotifEnv e)
    $ corsWithContentType
    $ logStdoutDev
    $ static
    $ app e
  where
    -- | Allow Content-Type header with values other then allowed by simpleCors.
    corsWithContentType = cors (const $ Just policy)
      where
        policy = simpleCorsResourcePolicy
          { corsRequestHeaders = ["Content-Type"] }
