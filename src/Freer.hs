{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Freer where

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Exception
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import GHC.Generics

class CoEff f where
  comap :: (a -> b) -> Eff (f a ': r) x -> Eff (f b ': r) x

class ContraEff f where
  conmap :: (b -> a) -> Eff (f a ': r) x -> Eff (f b ': r) x

class IsoEff f where
  isomap :: (a -> b) -> (b -> a) -> Eff (f a ': r) x -> Eff (f b ': r) x

eff :: (forall v. (v -> Eff (f b ': r) x) -> f a v -> Eff (f b ': r) x)
    -> Eff (f a ': r) x
    -> Eff (f b ': r) x
eff f = replaceRelay pure $ flip f

instance CoEff Writer where
  comap f = eff $ \arr -> \case
    Writer a -> send (Writer $ f a) >>= arr

instance ContraEff Reader where
  conmap f = eff $ \arr -> \case
    Reader -> send Reader >>= arr . f

instance IsoEff State where
  isomap f g = eff $ \arr -> \case
    Get   -> send Get >>= arr . g
    Put s -> send (Put $ f s) >>= arr

