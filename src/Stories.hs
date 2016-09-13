{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Stories where

import Data.Kind (Type)
import Control.Monad.Free
import Control.Comonad.Cofree

import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.CustomStar
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.List (Elem, Filter)




data Desirable = Desirable String deriving (Eq, Ord)
instance Show Desirable where
    show (Desirable name) = name

data Character = Character { characterName :: String } deriving (Eq, Ord)
instance Show Character where
    show (Character name) = name

data Opinion = Friend
             | Neutral
             | Enemy
             deriving (Eq, Show)

data Knowledge = ChangeOf ChangeResult deriving (Eq, Show)

data ChangeResult = ChangeResult Character ChangeType
    deriving (Eq, Show)
data ChangeType = Introduce
                | Die
                | Kill Character
                | Leave
                | Arrive
                | Learn Knowledge
                | Obtain
                | Want Desirable
                | Achieve Desirable
                | Feel Character Opinion
                deriving (Eq, Show)



data Sum f g a = InL (f a) | InR (g a) deriving Functor
type f :+: g = Sum f g
infixr 8 :+:

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = InL

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = InR . inj

data Product f g a = Product (f a) (g a) deriving Functor
type f :*: g = Product f g
infixr 9 :*:


$(singletons [d|
    data StoryCmd = ChangeCmd
                  | InterruptCmd
                  | MacguffinCmd
                  deriving Show
    |])

type Knot c a = Knotted c a a
type CoKnot c a = CoKnotted c a a

-- Fold a type-level list of functors into a right-associative data-type-a-la-carte.
type family Knotted (ctr :: (Type -> Type) -> (Type -> Type) -> Type -> Type)
                    -- ^The constructor for the a-la-cartedness.
                    (all :: [StoryCmd])
                    -- ^The entire type-level list.
                    (cont :: [StoryCmd]) :: Type -> Type
                    -- ^The thus-processed type-level list.
type instance Knotted ctr all (x ': '[]) = GetCmd x all
type instance Knotted ctr all (x ': (y ': ys)) = ctr (GetCmd x all) (Knotted ctr all (y ': ys))

type family CoKnotted (ctr :: (Type -> Type) -> (Type -> Type) -> Type -> Type)
                      -- ^The constructor for the a-la-cartedness.
                      (all :: [StoryCmd])
                      -- ^The entire type-level list.
                      (cont :: [StoryCmd]) :: Type -> Type
                      -- ^The thus-processed type-level list.
type instance CoKnotted ctr all (x ': '[]) = CoCmd x all
type instance CoKnotted ctr all (x ': (y ': ys)) = ctr (CoCmd x all) (CoKnotted ctr all (y ': ys))

-- Given a list of functors, return the functors in the opposite category.
type family CoList (all :: [StoryCmd]) (xs :: [StoryCmd]) :: [Type -> Type] where
    CoList all '[] = '[]
    CoList all (x ': xs) = CoCmd x all ': CoList all xs

-- Every functor to be used in our Story DSL must have an instance of this class.
class HasDSL (t :: StoryCmd) where
    data GetCmd t (all :: [StoryCmd]) :: Type -> Type
    data CoCmd  t (all :: [StoryCmd]) :: Type -> Type

instance HasDSL ChangeCmd where
    data GetCmd ChangeCmd all a = Change Character ChangeType (ChangeResult -> a) deriving Functor
    data CoCmd  ChangeCmd all a = CoChange
                                { changeH :: Character -> ChangeType -> (ChangeResult, a)
                                } deriving Functor


instance HasDSL InterruptCmd where
    data GetCmd InterruptCmd all a where
        Interrupt :: KnotStory (Filter ((:/=$) @@ 'InterruptCmd) all) x
                  -> KnotStory (Filter ((:/=$) @@ 'InterruptCmd) all) y
                  -> (y -> a)
                  -> GetCmd InterruptCmd all a
    data CoCmd  InterruptCmd all a where
        CoInterrupt :: { interruptH :: KnotStory (Filter ((:/=$) @@ 'InterruptCmd) all) x
                                    -> KnotStory (Filter ((:/=$) @@ 'InterruptCmd) all) y
                                    -> (y, a)
                       } -> CoCmd InterruptCmd all a
deriving instance Functor (GetCmd InterruptCmd k)
deriving instance Functor (CoCmd  InterruptCmd k)


instance HasDSL MacguffinCmd where
    data GetCmd MacguffinCmd all a = Macguffin (Desirable -> a) deriving Functor
    data CoCmd  MacguffinCmd all a = CoMacguffin
                                   { macguffinH :: (Desirable, a)
                                   } deriving Functor



type MyCmds        = '[ChangeCmd, MacguffinCmd]
type KnotStory   k = Free   (Knot Sum k)
type KnotCoStory k = Cofree (CoKnot Product k)
type Story         = KnotStory   MyCmds
type CoStory       = KnotCoStory MyCmds

