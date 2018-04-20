{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Backend.Database where


import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Tree (Tree (Node))

import Data.Acid
import Data.Default (def)
import Data.SafeCopy
import Data.Typeable
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Common

newtype Database = Database Motif
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Database)
$(deriveSafeCopy 0 'base ''Motif)
$(deriveSafeCopy 0 'base ''MomentTree)
$(deriveSafeCopy 0 'base ''UUID)
$(deriveSafeCopy 0 'base ''NodeState)
$(deriveSafeCopy 0 'base ''Moment)
$(deriveSafeCopy 0 'base ''Context)
$(deriveSafeCopy 0 'base ''Content)

writeState :: Motif -> Update Database ()
writeState newValue = put (Database newValue)

queryState :: Query Database Motif
queryState = do
  Database motif <- ask
  return motif

$(makeAcidic ''Database ['writeState, 'queryState])

openDb :: IO (AcidState Database)
openDb = openLocalState ini
  where
    ini = Database $ Motif "Hello" $ MomentTree initialNode
    initialNode = Node (uuid, def, MomentJournal [] "Hello World") []
    uuid = fromJust $ UUID.fromString "a6463901-6f36-43f5-96d8-e07b695d214d"

get :: AcidState Database -> IO Motif
get db = query db QueryState

put :: AcidState Database -> Motif -> IO ()
put db v = update db $ WriteState v
