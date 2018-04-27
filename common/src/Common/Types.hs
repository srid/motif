{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Tree
import Data.Typeable
import Data.UUID (UUID)
import GHC.Generics

import qualified Data.Text as T
import Servant.API

data NodeState = NodeState
  { _nodeStateDummy :: Bool
  , _nodeStateOpen :: Bool
  }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

type MotifTree a = Tree (UUID, NodeState, a)

instance Default NodeState where
  def = NodeState True True

-- TODO: Add application version (`git describe`)
data MotifEnv = MotifEnv
  { _motifEnvPort :: Int
  , _motifEnvDbPath :: FilePath
  }
  deriving (Generic, Show, Typeable, ToJSON, FromJSON)

-- | The main application type that is serialized over the wire
--   and in the database (acid-state)
newtype Motif = Motif
  { _motifTree :: MomentTree
  }
  deriving (Generic, Show, Typeable, ToJSON, FromJSON)

newtype MomentTree = MomentTree { unMomentTree :: MotifTree Moment }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

data Moment
  = MomentInbox Content  -- ^ Simplest content type; just text.
  | MomentJournal [Context] Content
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

newtype Content = Content { unContent :: Text }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

instance IsString Content where
  fromString = Content . T.pack

data Context
  = ContextNone
  | ContextChore
  | ContextReading
  | ContextIdea
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

class IsMoment a where
  getContext :: a -> [Context]
  getText :: a -> Text

instance IsMoment Moment where
  getContext = \case
    MomentInbox _ -> []
    MomentJournal v _ -> v
  getText = \case
    MomentInbox s -> unContent s
    MomentJournal _ s -> unContent s

instance IsMoment c => IsMoment (a, b, c) where
  getContext (_, _, x) = getContext x
  getText (_, _, x) = getText x

data MotifAction
  = MotifActionGet
  | MotifActionAddToInbox Text
  | MotifActionDelete UUID
  | MotifActionSetNodeState UUID NodeState
  deriving (Generic, Eq, Show, Ord, ToJSON, FromJSON)

-- | Response that gets sent over the wire.
type MotifResponse = (MotifEnv, Motif)

type MotifAPI = "motif"
  :> ReqBody '[JSON] MotifAction
  :> Post '[JSON] (Either Text MotifResponse)
