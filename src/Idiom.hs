{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Idiom where

import Data.Proxy

data Nat = Zero | Succ Nat

data NumArgs :: Nat -> * -> * where
    NAZero :: NumArgs Zero a
    NASucc :: NumArgs n b -> NumArgs (Succ n) (a -> b)

type family Listify (n :: Nat) arrows where
    Listify Zero t = [t]
    Listify (Succ n) (a -> b) = [a] -> Listify n b

listApply :: NumArgs n a -> [a] -> Listify n a
listApply NAZero fs = fs
listApply (NASucc na) fs = \args -> listApply na (apply fs args)
  where
    apply :: [a -> b] -> [a] -> [b]
    apply (f : fs) (x : xs) = f x : apply fs xs
    apply _ _ = []

type family CountArgs (f :: *) :: Nat where
    CountArgs (a -> b) = Succ (CountArgs b)
    CountArgs t        = Zero

class CNumArgs (numArgs :: Nat) (arrows :: *) where
    getNA :: NumArgs numArgs arrows
instance CNumArgs Zero a where
    getNA = NAZero
instance CNumArgs n b => CNumArgs (Succ n) (a -> b) where
    getNA = NASucc getNA

zipWith' :: forall f. CNumArgs (CountArgs f) f => f -> Listify (CountArgs f) f
zipWith' func = listApply (getNA :: NumArgs (CountArgs f) f) (repeat func)


class HasFnOfArity (ts :: [*]) where
    type FnOfArity                ts :: *
    type FFnOfArity (f :: * -> *) ts :: *
    idiom' :: Applicative f
          => Proxy ts
          -> f (FnOfArity ts)
          -> FFnOfArity f ts

instance HasFnOfArity (t ': '[]) where
    type FnOfArity    (t ': '[]) =   t
    type FFnOfArity f (t ': '[]) = f t
    idiom' _ = id

instance HasFnOfArity (t ': ts) => HasFnOfArity (a ': (t ': ts)) where
    type FnOfArity    (a ': (t ': ts)) = a -> FnOfArity    (t ': ts)
    type FFnOfArity f (a ': (t ': ts)) = a -> FFnOfArity f (t ': ts)
    idiom' _ f a = idiom' (Proxy @(t ': ts)) (f <*> pure a)

idiom :: forall f ts.
         ( HasFnOfArity ts
         , Applicative f
         )
      => Proxy        ts
      -> FnOfArity    ts
      -> FFnOfArity f ts
idiom p f = idiom' p (pure f :: f (FnOfArity ts))

test :: [Int]
test = idiom (Proxy @'[Int, Int, Int]) (+) 1 2 -- [3]

