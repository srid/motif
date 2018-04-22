{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common where

(<<$>>) :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

(<<$) :: (Functor f2, Functor f1) => a -> f1 (f2 b) -> f1 (f2 a)
v <<$ f = fmap (v <$) f
