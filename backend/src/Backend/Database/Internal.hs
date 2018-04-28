{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- NOTE: Do not rename or move this module, as the generated `WriteState` and
-- `QueryState` need to me under the same module namespace for acid-state to
-- work with an *existing* db. Doing a checkpoint may remove this requirement,
-- but check the acid-state documentation first.
module Backend.Database.Internal
  ( QueryState (..)
  , WriteState (..)
  ) where

import Control.Monad.Reader (ask)
import Control.Monad.State (put)
import Data.Acid
import Data.SafeCopy

import Data.UUID (UUID)

import Common.Types

-- These are explicitly orphan instances as the frontend doesn't need to know
-- about serialization or have `safe-copy` as a dependency.
$(deriveSafeCopy 2 'base ''Motif)
$(deriveSafeCopy 0 'base ''MotifNode)
$(deriveSafeCopy 0 'base ''UUID)
$(deriveSafeCopy 0 'base ''Moment)
$(deriveSafeCopy 0 'base ''Context)

writeState :: Motif -> Update Motif ()
writeState = put

queryState :: Query Motif Motif
queryState = ask

$(makeAcidic ''Motif ['writeState, 'queryState])
