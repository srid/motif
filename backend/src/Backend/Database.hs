{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Backend.Database
  ( openDb
  , closeDb
  , get
  , put
  ) where

import Data.Maybe (fromJust)
import Data.Tree (Tree (Node))

import Data.Acid (AcidState, closeAcidState, openLocalStateFrom, query, update)

import qualified Data.UUID as UUID

-- TODO: Import only Motif
import Common.Types (Moment (MomentInbox), Motif (Motif), MotifNode (MotifNode))

import Backend.Database.Internal (QueryState (QueryState), WriteState (WriteState))

openDb :: FilePath -> IO (AcidState Motif)
openDb = flip openLocalStateFrom ini
  where
    ini = Motif $ Node initialNode []
    initialNode = MotifNode uuid True $ MomentInbox "Inbox"
    uuid = fromJust $ UUID.fromString "a6463901-6f36-43f5-96d8-e07b695d214d"

closeDb :: AcidState Motif -> IO ()
closeDb = closeAcidState

get :: AcidState Motif -> IO Motif
get db = query db QueryState

put :: AcidState Motif -> Motif -> IO ()
put db v = update db $ WriteState v
