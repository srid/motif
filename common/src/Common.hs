{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default
import Data.List (nub)
import Data.String (IsString (fromString))
import Data.Text (Text)
import GHC.Generics

import qualified Data.Text as T
import Servant.API
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data Motif = Motif
  { _motifHello :: Text
  , _motifMomentTree :: MomentTree
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Tree a
  = Leaf a
  | Node NodeState a [Tree a]
  deriving (Generic, Eq, Ord, Show, Functor, ToJSON, FromJSON)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency [(1, Leaf <$> arbitrary), (2, node)]
    where
      node = do
        s <- arbitrary
        children <- scale (min 3) $ listOf arbitrary
        return $ Node def s children

foldUp :: a -> [Tree a] -> Tree a
foldUp = Node def

data NodeState = NodeState
  { _nodeStateDummy :: Bool
  , _nodeStateOpen :: Bool
  }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

instance Default NodeState where
  def = NodeState True True

newtype MomentTree = MomentTree { unMomentTree :: Tree Moment }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

newtype Content = Content { unContent :: Text }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

instance IsString Content where
  fromString = Content . T.pack

data Moment
  = MomentJournal [Context] Content
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

-- Future types!
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

instance Arbitrary Moment where
  arbitrary = do
    ctx <- fmap nub $ scale (min 3) $ listOf arbitrary
    s <- elements ["Buy milk", "File tax", "Pay rent to landlord", "Talk to Diane", "Call accountant", "Summary of vacation"]
    return $ MomentJournal ctx s

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

-- Hmm, this needs GADTs, unfortunately.
-- class FromJournal a where
--   fromJournal :: [Context] -> Text -> Bool -> a

-- instance FromJournal Moment where
--   fromJournal

type MotifAPI = "motif" :> Get '[JSON] (Either Text Motif)
