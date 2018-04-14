{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Lazy (Text)
import GHC.Generics

import Servant.API

data Motif = Motif
  { _motifHello :: Text
  , _motifHelloAgain :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type MotifAPI = "motif" :> Get '[JSON] (Either Text Motif)
