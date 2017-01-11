{-# LANGUAGE DeriveFunctor #-}

module Comonad where

import Data.Function
import Data.Semigroup
import Control.Comonad
import Control.Comonad.Store
import Debug.Trace

data Stream a = a :| Stream a deriving Functor

instance Comonad Stream where
  extract (a :| _) = a
  duplicate a@(_ :| as) = a :| duplicate as

data Zipper a = Zipper (Stream a) a (Stream a) deriving Functor

moveL :: Zipper a -> Zipper a
moveL (Zipper (l :| ls) a rs) = Zipper ls l $ a :| rs

moveR :: Zipper a -> Zipper a
moveR (Zipper ls a (r :| rs)) = Zipper (a :| ls) r rs

instance Comonad Zipper where
  extract (Zipper _ a _) = a
  duplicate = zipperf moveL moveR

coiter :: (a -> a) -> a -> Stream a
coiter f a = f a :| coiter f (f a)

instance ComonadApply Stream where
  (f :| fs) <@> (a :| as) = f a :| (fs <@> as)

instance ComonadApply Zipper where
  Zipper lf f rf <@> Zipper la a ra = Zipper (lf <@> la) (f a) (rf <@> ra)

zipperf :: (a -> a) -> (a -> a) -> a -> Zipper a
zipperf lf rf a = Zipper (coiter lf a) a (coiter rf a)

zipper :: a -> a -> a -> Zipper a
zipper l a r = Zipper (coiter id l) a (coiter id r)

stolist :: Stream a -> [a]
stolist (a :| as) = a : stolist as

tolist :: Zipper a -> [a]
tolist (Zipper _ a as) = stolist $ a :| as

wat :: Zipper (Zipper Integer -> Integer)
wat = zipperf (const $ const 1) (\f w -> extract (moveL w) + extract (moveL $ moveL w)) (const 1)

evaluate :: (ComonadApply w) => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate

main :: IO ()
main = print . take 10 . tolist $ evaluate wat

