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

import Data.Acid
import Data.Default (def)

import qualified Data.UUID as UUID

-- TODO: Import only Motif
import Common.Types (Moment (MomentInbox), MomentTree (MomentTree), Motif (Motif))

import Backend.Database.Internal (QueryState (QueryState), WriteState (WriteState))

openDb :: IO (AcidState Motif)
openDb = openLocalState ini
  where
    ini = Motif "Hello" $ MomentTree initialNode
    initialNode = Node (uuid, def, MomentInbox "First item") []
    uuid = fromJust $ UUID.fromString "a6463901-6f36-43f5-96d8-e07b695d214d"

closeDb :: AcidState Motif -> IO ()
closeDb = closeAcidState

get :: AcidState Motif -> IO Motif
get db = query db QueryState

put :: AcidState Motif -> Motif -> IO ()
put db v = update db $ WriteState v
