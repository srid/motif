{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common.Tree where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default
import Data.Text (Text)
import GHC.Generics

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

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

data NodeState = NodeState
  { _nodeStateDummy :: Bool
  , _nodeStateOpen :: Bool
  }
  deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

instance Default NodeState where
  def = NodeState True True
