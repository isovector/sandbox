{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Proxy (Proxy (..))
import GHC.TypeLits

data (:>) (a :: k) (b :: k')
infixr 9 :>

class HasPrintf a where
    type Printf a :: *
    printf' :: String -> Proxy a -> Printf a

instance KnownSymbol s => HasPrintf s where
    type Printf s = String
    printf' s p = s ++ symbolVal p

instance (Show t, HasPrintf rest) => HasPrintf ((t :: *) :> rest) where
    type Printf (t :> rest) = t -> Printf rest
    printf' s (_ :: Proxy (t :> rest)) = \v -> printf' (s ++ show v) (Proxy @rest)

instance (KnownSymbol s, HasPrintf rest) => HasPrintf (s :> rest) where
    type Printf (s :> rest) = Printf rest
    printf' s (_ :: Proxy (p :> rest)) = printf' (s ++ symbolVal (Proxy @p)) (Proxy @rest)

printf :: HasPrintf p => Proxy p -> Printf p
printf = printf' ""

x :: Proxy ("Hello" :> Int :> "Goodbye")
x = Proxy

main :: IO ()
main = putStrLn $ printf x 7

