{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Backend.Server (runServer, Env(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Default (def)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Tree
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

import Servant
import Servant.Server (hoistServer)

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Common

data Env = Env {}
  deriving (Eq, Show, Ord)

type AppM = ReaderT Env Handler

app :: Env -> Application
app e = serve motifAPI $ hoistServer motifAPI (`runReaderT` e) motifServer

motifAPI :: Proxy MotifAPI
motifAPI = Proxy

motifServer :: ServerT MotifAPI AppM
motifServer = getMotif :<|> postMotif
  where
    getMotif :: AppM (Either Text Motif)
    getMotif =
      liftIO $ Right <$> sample
    postMotif :: (UUID, NodeState) -> AppM (Either Text Motif)
    postMotif (_id', _collapsed) =
      liftIO $ Right <$> sample

sample :: IO Motif
sample = do
  let st = def :: NodeState
  u1 <- nextRandom
  let m1 = (u1, st,) $ MomentJournal [ContextFoo] "Hello world"
  u2 <- nextRandom
  let m2 = (u2, st,) $ MomentJournal [ContextFoo] "Buy milk"
  u3 <- nextRandom
  let m3 = (u3, st,) $ MomentJournal [ContextBar] "Catch a fish"
  return $ Motif "Hello" . MomentTree $ Node m1 [Node m2 [Node m1 [], Node m3 []], Node m3 []]

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
