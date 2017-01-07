{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Comonad where

import ArbitraryInstances
import Data.Monoid ((<>))
import Laws
import Control.Comonad
import Test.QuickCheck
import Test.QuickCheck.Function
import Language.Haskell.Codo


instance (Arbitrary s, CoArbitrary s, Arbitrary a) => Arbitrary (Store s a) where
  arbitrary = Store <$> arbitrary <*> arbitrary


data Store s a = Store (s -> a) s
  deriving (Eq, Show)

instance Functor (Store s) where
  fmap f (Store s ix) = Store (f . s) ix

instance Comonad (Store s) where
  extract (Store s ix) = s ix
  extend f w@(Store s ix) = Store (\sb -> f $ seek sb w) ix

instance Monoid s => Applicative (Store s) where
  pure a = Store (const a) mempty
  Store fs fix <*> Store as aix = Store (fs <*> as) (fix <> aix)


seek :: s -> Store s a -> Store s a
seek ix (Store s _) = Store s ix

peek :: s -> Store s a -> a
peek ix (Store s _) = s ix

pos :: Store s a -> s
pos (Store _ ix) = ix


data Env e a = Env e a
  deriving (Eq, Show)

instance Functor (Env e) where
  fmap f (Env e a) = Env e $ f a

instance Comonad (Env e) where
  extract (Env _ a) = a
  duplicate w@(Env e a) = Env e w

instance (Monoid e) => Applicative (Env e) where
  pure = Env mempty
  Env fe fa <*> Env e a = Env (fe <> e) (fa a)

ask :: Env e a -> e
ask (Env e _) = e

adder :: Store Int Int -> Int
adder = [codo| w =>
        let p = pos w
        peek (p - 1) w + peek (p + 1) w + extract w
  |]

test :: Store Int Int -> Store Int Int
test = [codo| x =>
       w <- adder x
       w
  |]

