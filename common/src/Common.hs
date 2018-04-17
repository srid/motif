{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (nub)
import Data.String (IsString (fromString))
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import qualified Data.Text as T
import Servant.API

import Common.Tree

data Motif = Motif
  { _motifHello :: Text
  , _motifMomentTree :: MomentTree
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype MomentTree = MomentTree { unMomentTree :: Tree Moment }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

data Moment
  = MomentJournal [Context] Content
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

instance Arbitrary Moment where
  arbitrary = do
    ctx <- fmap nub $ scale (min 3) $ listOf arbitrary
    s <- elements ["Buy milk", "File tax", "Pay rent to landlord", "Talk to Diane", "Call accountant", "Summary of vacation"]
    return $ MomentJournal ctx s

newtype Content = Content { unContent :: Text }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

instance IsString Content where
  fromString = Content . T.pack

instance Arbitrary Content where
  arbitrary = fmap (Content . T.pack) <$> listOf $ elements ['a' .. 'z']

data Context
  = ContextNone
  | ContextFoo
  | ContextBar
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

instance Arbitrary Context where
  arbitrary = frequency [
      (1, return ContextNone)
    , (2, return ContextFoo)
    , (4, return ContextBar)
    ]

class IsMoment a where
  getContext :: a -> [Context]
  getText :: a -> Text

instance IsMoment Moment where
  getContext = \case
    MomentJournal v _ -> v
  getText = \case
    MomentJournal _ s -> unContent s

type MotifAPI = "motif" :> Get '[JSON] (Either Text Motif)

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

