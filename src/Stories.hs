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

module Stories (main) where

import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Kind (Type)
import Data.Maybe (isJust)

import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.List (Elem, Filter)

import Test.QuickCheck




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


class Joinable (fs :: [Type -> Type]) where
    data Joined fs :: Type -> Type

instance Joinable '[] where
    data Joined '[] a = JNil Void deriving Functor

instance Joinable (f ': fs) where
    data Joined (f ': fs) a = Functor f => Here (f a)
                            | There (Joined fs a)
deriving instance Functor (Joined fs) => Functor (Joined (f ': fs))

class Injectable (f :: Type -> Type) (fs :: [Type -> Type]) where
    inj :: f a -> Joined fs a

instance Functor f => Injectable f (f ': fs) where
    inj = Here

instance {-# OVERLAPPABLE #-} Injectable f fs => Injectable f (g ': fs) where
    inj = There . inj

class Outjectable (f :: Type -> Type) (fs :: [Type -> Type]) where
    outj :: Joined fs a -> Maybe (f a)

instance Outjectable f (f ': fs) where
    outj (Here f)  = Just f
    outj (There _) = Nothing

instance {-# OVERLAPPABLE #-} Outjectable f fs => Outjectable f (g ': fs) where
    outj (There f) = outj f
    outj (Here _ ) = Nothing

class (Joinable fs, Injectable f fs, Outjectable f fs, Functor (Joined fs)) => (f :: Type -> Type) :<: (fs :: [Type -> Type])
instance (Joinable fs, Injectable f fs, Outjectable f fs, Functor (Joined fs)) => (f :<: fs)


data Product f g a = Product (f a) (g a) deriving Functor
type f :*: g = Product f g
infixr 9 :*:


$(singletons [d|
    data StoryCmd = ChangeCmd
                  | InterruptCmd
                  | MacguffinCmd
                  deriving Show
    |])

class CanMapToFunctorList (cmds :: [StoryCmd]) (all :: [StoryCmd]) where
    type MapToFunctorList cmds all :: [Type -> Type]

instance CanMapToFunctorList '[] all where
    type MapToFunctorList '[] all = '[]

instance HasDSL f => CanMapToFunctorList (c ': cs) all where
    type MapToFunctorList (c ': cs) all = GetCmd c all ': MapToFunctorList cs all

type CoKnot c a = CoKnotted c a a

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
type Functors    k = MapToFunctorList k k
type JoinedList  k = Joined (Functors k)
type KnotStory   k = Free   (JoinedList k)
type KnotCoStory k = Cofree (CoKnot Product k)
type Story         = KnotStory   MyCmds
type CoStory       = KnotCoStory MyCmds

change :: forall cmds. (GetCmd ChangeCmd cmds :<: Functors cmds)
       => Proxy cmds -> Character -> ChangeType -> KnotStory cmds ChangeResult
change _ c ct = liftF $ inj (Change c ct id :: GetCmd ChangeCmd cmds ChangeResult)

-- TODO(sandy): how can we define this for polymorphic cmds?
change' :: Character -> ChangeType -> Story ChangeResult
change' = change (Proxy @MyCmds)

test :: Story ()
test = do
    change' (Character "") Die
    return ()


injOutj_prop :: forall fs f a. (f :<: fs) => Proxy fs -> f a -> Bool
injOutj_prop _ fa = isJust $ (outj (inj fa :: Joined fs a) :: Maybe (f a))

main = quickCheck (injOutj_prop (Proxy @'[[], Proxy, Maybe, (,) Int]) :: Maybe Int -> Bool)

