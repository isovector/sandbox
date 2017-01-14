{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns  #-}

module Comonad where

import Data.Function
import Data.Semigroup
import Control.Comonad

data Stream a = a :> Stream a deriving Functor

instance Comonad Stream where
  extract (a :> _) = a
  duplicate a@(_ :> as) = a :> duplicate as

newtype Embed f a = Embed { unembed :: f (f a) } deriving Functor

instance Comonad f => Comonad (Embed f) where
  extract = extract . extract . unembed
  duplicate (unembed -> w) = Embed $ fmap (Embed . duplicate) <$> duplicate w

instance (ComonadApply f, Applicative f) => Applicative (Embed f) where
  pure = Embed . pure . pure
  (<*>) = (<@>)

instance ComonadApply f => ComonadApply (Embed f) where
  Embed f <@> Embed a = Embed $ (<@>) <$> f <@> a

data Zipper a = Zipper (Stream a) a (Stream a) deriving Functor

instance Applicative Zipper where
  pure a = zipper id id a
  (<*>) = (<@>)

type Sheet = Embed Zipper

moveL :: Zipper a -> Zipper a
moveL (Zipper (l :> ls) a rs) = Zipper ls l $ a :> rs

moveR :: Zipper a -> Zipper a
moveR (Zipper ls a (r :> rs)) = Zipper (a :> ls) r rs

instance Comonad Zipper where
  extract (Zipper _ a _) = a
  duplicate = zipper moveL moveR

coiter :: (a -> a) -> a -> Stream a
coiter f a = f a :> coiter f (f a)

instance ComonadApply Stream where
  (f :> fs) <@> (a :> as) = f a :> (fs <@> as)

instance ComonadApply Zipper where
  Zipper lf f rf <@> Zipper la a ra = Zipper (lf <@> la) (f a) (rf <@> ra)

zipper :: (a -> a) -> (a -> a) -> a -> Zipper a
zipper lf rf a = Zipper (coiter lf a) a (coiter rf a)

stolist :: Stream a -> [a]
stolist (a :> as) = a : stolist as

tolist :: Zipper a -> [a]
tolist (Zipper _ a as) = stolist $ a :> as

wat :: Zipper (Zipper Integer -> Integer)
wat = zipper id (\f w -> extract (moveL w) + extract (moveL $ moveL w)) (const 1)

inject :: a -> Zipper a -> Zipper a
inject a (Zipper ls _ rs) = Zipper ls a rs

sheet :: Sheet (Sheet Integer -> Integer)
sheet = Embed $ fmap (inject $ const 1) (inject (pure $ const 1) base)
  where
    base = unembed $ pure go
    go :: Sheet Integer -> Integer
    go (unembed -> w) = up + left
      where
        up   = extract . extract $ moveL w
        left = extract . moveL $ extract w

pascal :: Zipper (Zipper Integer)
pascal = unembed $ kfix sheet

moveRN :: Integer -> Zipper a -> Zipper a
moveRN (fromIntegral -> n) = head . drop n . iterate moveR

triangle :: Integer -> Integer
triangle n = extract $ moveRN n . extract . moveRN n $ pascal


main :: IO ()
main = print . take 10 . tolist $ kfix wat

