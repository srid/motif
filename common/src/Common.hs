{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Tree
import GHC.Generics

import qualified Data.Text as T
import Servant.API


newtype Id = Id { unId :: Text }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

data Motif = Motif
  { _motifHello :: Text
  , _motifMomentTree :: MomentTree
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype MomentTree = MomentTree { unMomentTree :: Tree Moment }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

data Moment
  = MomentJournal [Context] Content
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

newtype Content = Content { unContent :: Text }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

instance IsString Content where
  fromString = Content . T.pack

data Context
  = ContextNone
  | ContextFoo
  | ContextBar
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

class IsMoment a where
  getContext :: a -> [Context]
  getText :: a -> Text

instance IsMoment Moment where
  getContext = \case
    MomentJournal v _ -> v
  getText = \case
    MomentJournal _ s -> unContent s

type MotifAPI =
  "motif" :> Get '[JSON] (Either Text Motif)
  :<|> "collapse-state" :> ReqBody '[JSON] (Id, Bool) :> Post '[JSON] (Either Text Motif)

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

