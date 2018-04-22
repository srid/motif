{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common.Types where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Default
import Data.Monoid ((<>))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Tree
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
  deriving (Generic, Eq, Ord, Show, Read, ToJSON, FromJSON)

type MotifTree a = Tree (UUID, NodeState, a)

instance Default NodeState where
  def = NodeState True True

newtype Motif = Motif
  { _motifTree :: MomentTree
  }
  deriving (Generic, Show, Read, Typeable, ToJSON, FromJSON)

newtype MomentTree = MomentTree { unMomentTree :: MotifTree Moment }
  deriving (Generic, Eq, Show, Read, ToJSON, FromJSON)

-- FIXME: GADT is more appropriate here.
data Moment
  = MomentInbox Content  -- ^ Simplest content type; just text.
  | MomentJournal [Context] Content
  deriving (Generic, Eq, Ord, Show, Read, ToJSON, FromJSON)

newtype Content = Content { unContent :: Text }
  deriving (Generic, Eq, Ord, Show, Read, ToJSON, FromJSON)

instance IsString Content where
  fromString = Content . T.pack

data Context
  = ContextNone
  | ContextChore
  | ContextReading
  | ContextIdea
  deriving (Generic, Eq, Ord, Show, Read, ToJSON, FromJSON)

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
  deriving (Generic, Eq, Show, Read, Ord, ToJSON, FromJSON)

type MotifAPI = "motif"
  :> ReqBody '[HaskellType] MotifAction
  :> Post '[HaskellType] (Either Text Motif)

data HaskellType

instance Accept HaskellType where
  contentType _ = "text" // "haskell" /: ("charset", "utf-8")

instance Show a => MimeRender HaskellType a where
  mimeRender _ = BC.pack . show

instance Read a => MimeUnrender HaskellType a where
  mimeUnrender _ = f . BC.unpack
    where
      f s = maybe (Left ("Bad text: " <> s)) Right $ readMaybe s

--------------------
--- Future types!
data MomentOtherExamples
  = MomentFoodLog [Context] [(Food, Int)]
  | MomentActualism [Context] Feeling Content
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)
data Food
  = Coffee
  | Croissant
  | BeefRibeyeGrams
  | LambBurgerGrams
  | Egg
  | Butter
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)
data Feeling
  = Terrible
  | Bad
  | Neutral
  | Good
  | Great
  | Perfect
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)
