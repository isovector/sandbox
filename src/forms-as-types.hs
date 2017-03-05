{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module FormTypes where

import Data.Profunctor
import Control.Monad.Amb
import BasePrelude

data Void

newtype DoubleNot a = DoubleNot { unDoubleNot :: (a -> Void) -> Void } deriving Functor

getNot :: WithBullshit a => a -> DoubleNot (Bullshit a)
getNot a = DoubleNot $ \f -> f $ toBullshit a

runNot :: WithBullshit a => DoubleNot (Bullshit a) -> a
runNot (DoubleNot dn) = undefined

absurd :: Void -> a
absurd v = case v of {}

vacuous :: Functor f => f Void -> f a
vacuous = fmap absurd

-- http://math.stackexchange.com/questions/1018185/proof-of-a-theorem-in-hilberts-system
eliminate :: DoubleNot a -> a
eliminate = undefined
  where
    hilbert1 :: a -> b -> a
    hilbert1 = const

    hilbert2 :: (a -> b -> c) -> (a -> b) -> (a -> c)
    hilbert2 = ap

    hilbert3 :: (Not b -> Not a) -> (Not b -> a) -> b
    hilbert3 nbna nba = error "cannot possibly be constructive"

type Not a = a -> Void

type family Bullshit t where
  Bullshit (a -> b) = DoubleNot (Bullshit a) -> DoubleNot (Bullshit b)
  Bullshit t = t

class WithBullshit t where
  toBullshit :: t -> Bullshit t
  fromBullshit :: Bullshit t -> t

instance (WithBullshit a, WithBullshit b) => WithBullshit (a -> b) where
  toBullshit f = dimap (fmap fromBullshit) (fmap toBullshit) $ \dnba -> fmap f dnba
  fromBullshit f = dimap getNot runNot f

instance (Bullshit t ~ t) => WithBullshit t where
  toBullshit = id
  fromBullshit = id



abort :: AmbT r m a
abort = mzero

pyTriple :: (Num t, Ord t) => t -> Amb r (t, t, t)
pyTriple n = do
  a <- anIntegerBetween 1 n
  b <- anIntegerBetween (a + 1) n
  c <- anIntegerBetween (b + 1) n
  unless (a*a + b*b == c*c) abort
  return (a,b,c)

val = allValues $ pyTriple 20

