{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns  #-}

module Comonad where

import Data.Function
import Data.Semigroup
import Control.Comonad
import Control.Comonad.Store
import Debug.Trace

data Stream a = a :> Stream a deriving Functor

instance Comonad Stream where
  extract (a :> _) = a
  duplicate a@(_ :> as) = a :> duplicate as

data Embed f a = Embed { unembed :: f (f a) } deriving Functor

instance Comonad f => Comonad (Embed f) where
  extract = extract . extract . unembed
  duplicate (unembed -> w) = Embed $ fmap (Embed . duplicate) <$> duplicate w

instance ComonadApply f => ComonadApply (Embed f) where
  Embed f <@> Embed a = Embed $ (<@>) <$> f <@> a

data Zipper a = Zipper (Stream a) a (Stream a) deriving Functor

type Sheet = Embed Zipper

moveL :: Zipper a -> Zipper a
moveL (Zipper (l :> ls) a rs) = Zipper ls l $ a :> rs

moveR :: Zipper a -> Zipper a
moveR (Zipper ls a (r :> rs)) = Zipper (a :> ls) r rs

instance Comonad Zipper where
  extract (Zipper _ a _) = a
  duplicate = zipperf moveL moveR

coiter :: (a -> a) -> a -> Stream a
coiter f a = f a :> coiter f (f a)

instance ComonadApply Stream where
  (f :> fs) <@> (a :> as) = f a :> (fs <@> as)

instance ComonadApply Zipper where
  Zipper lf f rf <@> Zipper la a ra = Zipper (lf <@> la) (f a) (rf <@> ra)

zipperf :: (a -> a) -> (a -> a) -> a -> Zipper a
zipperf lf rf a = Zipper (coiter lf a) a (coiter rf a)

zipper :: a -> a -> a -> Zipper a
zipper l a r = Zipper (coiter id l) a (coiter id r)

stolist :: Stream a -> [a]
stolist (a :> as) = a : stolist as

tolist :: Zipper a -> [a]
tolist (Zipper _ a as) = stolist $ a :> as

wat :: Zipper (Zipper Integer -> Integer)
wat = zipperf id (\f w -> extract (moveL w) + extract (moveL $ moveL w)) (const 1)

sheet :: Sheet (Sheet Integer -> Integer)
sheet = Embed . zipperf id id $ zipperf id (const go) $ const 1
  where
    go :: Sheet Integer -> Integer
    go (unembed -> w) = up + left
      where
        up   = extract . extract $ moveL w
        left = extract . moveL $ extract w

pascal :: Sheet Integer
pascal = kfix sheet

moveRN :: Integer -> Zipper a -> Zipper a
moveRN (fromIntegral -> n) = head . drop n . iterate moveR

triangle :: Integer -> Integer
triangle n = extract $ moveRN n . extract . moveRN n $ unembed pascal


main :: IO ()
main = print . take 10 . tolist $ kfix wat

