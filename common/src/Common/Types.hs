{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common.Types where

import Data.Text (Text)
import Data.Tree
import Data.Typeable
import Data.UUID (UUID)
import GHC.Generics

import Servant.API

import Common.Internal (HaskellType)

-- TODO: Add application version (`git describe`)
data MotifEnv = MotifEnv
  { _motifEnvPort :: Int
  , _motifEnvDbPath :: FilePath
  }
  deriving (Generic, Show, Typeable, Read)

data MotifNode = MotifNode
  { _motifNodeID :: UUID
  , _motifNodeOpen :: Bool
  , _motifNodeValue :: Moment
  }
  deriving (Generic, Eq, Ord, Show, Read)

-- | The main application type that is serialized over the wire
--   and in the database (acid-state)
newtype Motif = Motif
  { _motifTree :: Tree MotifNode
  }
  deriving (Generic, Show, Typeable, Read)

type Content = Text

data Moment
  = MomentInbox Content  -- ^ Simplest content type; just text.
  | MomentJournal [Context] Content
  deriving (Generic, Eq, Ord, Show, Read)

data Context
  = ContextNone
  | ContextChore
  | ContextReading
  | ContextIdea
  deriving (Generic, Eq, Ord, Show, Read)

class IsMoment a where
  getContext :: a -> [Context]
  getText :: a -> Text

instance IsMoment Moment where
  getContext = \case
    MomentInbox _ -> []
    MomentJournal v _ -> v
  getText = \case
    MomentInbox s -> s
    MomentJournal _ s -> s

instance IsMoment MotifNode where
  getContext = getContext . _motifNodeValue
  getText = getText . _motifNodeValue

data MotifAction
  = MotifActionGet
  | MotifActionAddToInbox Content
  | MotifActionDelete UUID
  | MotifActionSetOpen UUID Bool
  deriving (Generic, Eq, Show, Ord, Read)

-- | Response that gets sent over the wire.
-- TODO: Fix the type alias madness
type MotifResponse = (MotifEnv, Tree MotifNode)

type MotifAPI = "motif"
  :> ReqBody '[HaskellType] MotifAction
  :> Post '[HaskellType] (Either Text MotifResponse)
