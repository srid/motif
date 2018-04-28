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
        let node = MotifNode uuid True $ MomentInbox s
        pure $ addLevel2Child (Node node []) tree0
      MotifActionDelete id' -> editMotif $ \tree0 ->
        -- FIXME: don't use head
        pure $ head $ findAndMap' (isUUID id') (const Nothing) [tree0]
      MotifActionSetOpen id' isOpen -> editMotif $ \tree0 ->
        pure $ findAndMap (isUUID id') (\(Node node xs) -> Node (node { _motifNodeOpen = isOpen}) xs) tree0
    editMotif f = withConfig $ \db -> do
      tree0 <- getTree db
      tree1 <- f tree0
      putTree db tree1
      return tree1
    withConfig f = do
      config <- ask
      -- TODO: Will need to handle errors (Left) at some point.
      liftIO $ Right . (_configMotifEnv config, ) <$> f (_configDb config)
    getTree = fmap _motifTree . Database.get
    putTree db = Database.put db . Motif

-- | Add a node to the top-level level1 node.
addLevel2Child :: Tree a -> Tree a -> Tree a
addLevel2Child n (Node v xs) = Node v $ n : xs

-- | TODO: Use this instead of above.
addChild :: UUID -> MotifNode -> Tree MotifNode -> Tree MotifNode
addChild u v = findAndMap (isUUID u) (\(Node x xs) -> Node x $ newNode v : xs)
  where newNode x = Node x []

findAndMap :: (a -> Bool) -> (Tree a -> Tree a) -> Tree a -> Tree a
findAndMap p f t@(Node v ns)
  | p v = f t
  -- FIXME: should operate only on one child.
  | otherwise = Node v $ map (findAndMap p f) ns

findAndMap' :: (a -> Bool) -> (Tree a -> Maybe (Tree a)) -> Forest a -> Forest a
findAndMap' _ _ [] = []
findAndMap' p f t' = catMaybes $ fmap g t'
  where g t@(Node v ns)
          | p v = f t
          | otherwise = Just $ Node v $ findAndMap' p f ns

isUUID :: UUID -> MotifNode -> Bool
isUUID x n = x == _motifNodeID n

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
