{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Coeff where

import BasePrelude
import Control.Monad.Freer
import Data.Functor.Identity

data KVStore k v a where
  Get    :: k -> KVStore k v v
  Put    :: k -> v -> KVStore k v ()
  Modify :: k -> (v -> v) -> KVStore k v ()

get :: Member (KVStore k v) r => k -> Eff r v
get = send . Get

put :: Member (KVStore k v) r => k -> v -> Eff r ()
put = (send .) . Put

modify :: Member (KVStore k v) r => k -> (v -> v) -> Eff r ()
modify = (send .) . Modify

class Day f g where
  convolute :: (a -> b -> c) -> f a -> g b -> c

instance Day Identity Identity where
  convolute f (runIdentity -> a) (runIdentity -> b) = f a b

instance Day ((,) x) ((->) x) where
  convolute f px ax = convolute (flip f) ax px

instance Day ((->) x) ((,) x)  where
  convolute f ax = uncurry $ f . ax


data CoKVStore k v a = CoKVStore
  { coget    :: k -> (v, a)
  , coput    :: k -> v -> a
  , comodify :: k -> (v -> v) -> a
  }

instance Day (CoKVStore k v) (KVStore k v) where
  convolute (flip -> f) CoKVStore {..} = \case
    Get k       -> uncurry f $ coget k
    Put k v     -> uncurry f . pure $ coput k v
    Modify k vf -> uncurry f . pure $ comodify k vf

-- coKVStore :: CoKVStore r k v
-- coKVStore = fix $ \self ->
--   CoKVStore
--   { comodify = \k f -> undefined
--   }


