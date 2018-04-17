{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Backend.Server (runServer, Env(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Monoid ((<>))
import Data.Text (Text)

import Servant
import Servant.Server (hoistServer)
import Test.QuickCheck (generate)
import Test.QuickCheck.Arbitrary (arbitrary)

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Common
import Common.Tree

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
    getMotif = do
      liftIO $ Right <$> sample
    postMotif :: (Id, Bool) -> AppM (Either Text Motif)
    postMotif (_id', _collapsed) = do
      liftIO $ Right <$> sample

sample :: IO Motif
sample = Motif "Hello" . MomentTree <$> generate arbitrary
-- TODO: ^ start using real data.

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
