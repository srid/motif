{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Common.Types where

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
  deriving (Generic, Eq, Ord, Show, Read)

type MotifTree a = Tree (UUID, NodeState, a)

instance Default NodeState where
  def = NodeState True True

newtype Motif = Motif
  { _motifTree :: MomentTree
  }
  deriving (Generic, Typeable)

deriving instance Show Motif
deriving instance Read Motif

data MomentTree
  = forall a. (Read a, Show a, IsMoment a)
  => MomentTree { unMomentTree :: Tree (UUID, NodeState, a) }

deriving instance Show MomentTree
deriving instance Read MomentTree

data Moment a where
  MkMoment :: a -> Moment a
  deriving (Generic, Eq, Ord)

deriving instance Show a => Show (Moment a)
deriving instance Read a => Read (Moment a)

newtype Inbox = Inbox Content
  deriving (Generic, Eq, Ord, Show, Read)

data Journal = Journal [Context] Content
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

instance IsMoment Inbox where
  getContext = const mempty
  getText (Inbox c) = unContent c

instance IsMoment Journal where
  getContext (Journal ctx _) = ctx
  getText  (Journal _ c) = unContent c

instance IsMoment a => IsMoment (Moment a) where
  getContext (MkMoment x) = getContext x
  getText (MkMoment x) = getText x

instance IsMoment c => IsMoment (a, b, c) where
  getContext (_, _, x) = getContext x
  getText (_, _, x) = getText x

data MotifAction
  = MotifActionGet
  | MotifActionAddToInbox Text
  | MotifActionDelete UUID
  | MotifActionSetNodeState UUID NodeState
  deriving (Generic, Eq, Show, Read, Ord)

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
  deriving (Generic, Eq, Ord, Show)
data Food
  = Coffee
  | Croissant
  | BeefRibeyeGrams
  | LambBurgerGrams
  | Egg
  | Butter
  deriving (Generic, Eq, Ord, Show)
data Feeling
  = Terrible
  | Bad
  | Neutral
  | Good
  | Great
  | Perfect
  deriving (Generic, Eq, Ord, Show)
