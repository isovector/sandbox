{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Idiom where

import Data.Proxy

class HasFnOfArity (ts :: [*]) where
    type FnOfArity ts :: *
    type FFnOfArity (f :: * -> *) ts :: *
    idiom' :: Applicative f
          => Proxy ts
          -> f (FnOfArity ts)
          -> FFnOfArity f ts

instance HasFnOfArity (t ': '[]) where
    type FnOfArity    (t ': '[]) = t
    type FFnOfArity f (t ': '[]) = f t
    idiom' _ f = f

instance HasFnOfArity (t ': ts) => HasFnOfArity (a ': (t ': ts)) where
    type FnOfArity    (a ': (t ': ts)) = a -> FnOfArity    (t ': ts)
    type FFnOfArity f (a ': (t ': ts)) = a -> FFnOfArity f (t ': ts)
    idiom' _ f = \a -> idiom' (Proxy @(t ': ts)) (f <*> pure a)

idiom :: forall f ts.
         ( HasFnOfArity ts
         , Applicative f
         )
      => Proxy ts
      -> FnOfArity ts
      -> FFnOfArity f ts
idiom p f = idiom' p (pure f :: f (FnOfArity ts))

test :: [Int]
test = idiom (Proxy @'[Int, Int, Int]) (+) 1 2 -- [3]

