{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common.Types where

import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Default
import Data.Monoid ((<>))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Tree
import Data.Tree.Zipper
import Data.Typeable
import Data.UUID (UUID)
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Text.Read (readMaybe)

import qualified Data.Text as T
import Servant.API

data NodeState = NodeState
  { _nodeStateDummy :: Bool
  , _nodeStateOpen :: Bool
  }
  deriving (Generic, Eq, Ord, Show, Read)

type MotifTree a = Tree (UUID, NodeState, a)

instance Default NodeState where
  def = NodeState True True

-- TODO: Add application version (`git describe`)
data MotifEnv = MotifEnv
  { _motifEnvPort :: Int
  , _motifEnvDbPath :: FilePath
  }
  deriving (Generic, Show, Typeable, Read)

-- | The main application type that is serialized over the wire
--   and in the database (acid-state)
newtype Motif = Motif
  { _motifTree :: MomentTree
  }
  deriving (Generic, Show, Typeable, Read)

newtype MomentTree = MomentTree { unMomentTree :: MotifTree Moment }
  deriving (Generic, Eq, Show, Read)

data Moment
  = MomentInbox Content  -- ^ Simplest content type; just text.
  | MomentJournal [Context] Content
  deriving (Generic, Eq, Ord, Show, Read)

newtype Content = Content { unContent :: Text }
  deriving (Generic, Eq, Ord, Show, Read)

instance IsString Content where
  fromString = Content . T.pack

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
  deriving (Generic, Eq, Show, Ord, Read)

-- | Response that gets sent over the wire.
-- TODO: Fix the type alias madness
type MotifResponse = (MotifEnv, TreePos Full (UUID, NodeState, Moment))

type MotifAPI = "motif"
  :> ReqBody '[HaskellType] MotifAction
  :> Post '[HaskellType] (Either Text MotifResponse)


data HaskellType

instance Accept HaskellType where
  contentType _ = "text" // "haskell" /: ("charset", "utf-8")

instance Show a => MimeRender HaskellType a where
  mimeRender _ = BC.pack . show

instance Read a => MimeUnrender HaskellType a where
  mimeUnrender _ = f . BC.unpack
    where
      f s = maybe (Left ("Bad text: " <> s)) Right $ readMaybe s
