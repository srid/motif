{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Internal where

import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Monoid ((<>))
import Network.HTTP.Media ((//), (/:))
import Text.Read (readMaybe)

import Servant.API

data HaskellType

instance Accept HaskellType where
  contentType _ = "text" // "haskell" /: ("charset", "utf-8")

instance Show a => MimeRender HaskellType a where
  mimeRender _ = BC.pack . show

instance Read a => MimeUnrender HaskellType a where
  mimeUnrender _ = f . BC.unpack
    where
      f s = maybe (Left ("Bad text: " <> s)) Right $ readMaybe s
