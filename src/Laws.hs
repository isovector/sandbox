{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ViewPatterns #-}

module Laws where

import Test.QuickCheck
import Test.QuickCheck.Function

monadAssocProp :: forall m a b c. (Monad m, Eq (m c)) => m a -> Fun a (m b) -> Fun b (m c) -> Bool
monadAssocProp x (apply -> f) (apply -> g) = ((x >>= f) >>= g) == (x >>= (\x' -> f x' >>= g))

monadLeftIdProp :: forall m a b. (Monad m, Eq (m b)) => a -> Fun a (m b) -> Bool
monadLeftIdProp x (apply -> f) = (return x >>= f) == (f x)

monadRightIdProp :: forall m a. (Monad m, Eq (m a)) => m a -> Bool
monadRightIdProp x = (x >>= return) == x
