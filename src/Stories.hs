{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
{-# LANGUAGE ViewPatterns #-}

module Stories (main) where

import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Kind (Constraint, Type)
import Data.Maybe (isJust)

import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.List (Elem, Filter)

-- TODO(sandy): remove this when you figure out how to write proofs
import Unsafe.Coerce

import Test.QuickCheck


type family (<=>) (c :: k -> Constraint) (as :: [k]) where
    (<=>) c '[k] = c k
    (<=>) c (h ': t) = (c h, (<=>) c t)
infixl 9 <=>



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

class Summable (fs :: [Type -> Type]) where
    data Summed fs :: Type -> Type

instance Summable '[] where
    data Summed '[] a = SummedNil Void deriving Functor

instance Summable (f ': fs) where
    data Summed (f ': fs) a = Functor f => Here !(f a)
                            | There !(Summed fs a)
deriving instance Functor (Summed fs) => Functor (Summed (f ': fs))

class Injectable (f :: Type -> Type) (fs :: [Type -> Type]) where
    inj :: f a -> Summed fs a
    outj :: Summed fs a -> Maybe (f a)

instance Functor f => Injectable f (f ': fs) where
    inj = Here
    outj (Here f)  = Just f
    outj (There _) = Nothing

instance {-# OVERLAPPABLE #-} Injectable f fs => Injectable f (g ': fs) where
    inj = There . inj
    outj (Here _ ) = Nothing
    outj (There f) = outj f

class ( Summable fs
      , Injectable f fs
      , Functor (Summed fs)
      ) => (f :: Type -> Type) :<: (fs :: [Type -> Type])
instance ( Summable fs
         , Injectable f fs
         , Functor (Summed fs)
         ) => (f :<: fs)


data Product f g a = Product (f a) (g a) deriving Functor
type f :*: g = Product f g
infixr 9 :*:


$(singletons [d|
    data StoryCmd = ChangeCmd
                  | InterruptCmd
                  | MacguffinCmd
                  deriving (Show, Eq, Ord)
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
    refined :: ( t :<+: cmds
               , t :<+: (Filter ((:/=$) @@ cmd) cmds)
               , (t :== cmd) ~ 'False
               )
            => SStoryCmd cmd
            -> (forall cmds a. GetCmd cmd cmds a -> KnotStory (Filter ((:/=$) @@ cmd) cmds) a)
            -> GetCmd t cmds a
            -> GetCmd t (Filter ((:/=$) @@ cmd) cmds) a


instance HasDSL ChangeCmd where
    data GetCmd ChangeCmd all a = Change Character ChangeType (ChangeResult -> a) deriving Functor
    data CoCmd  ChangeCmd all a = CoChange
                                { changeH :: Character -> ChangeType -> (ChangeResult, a)
                                } deriving Functor
    refined _ _ (Change c ct x) = Change c ct x


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
    -- TODO(sandy): remove the `unsafeCoerce` when you figure out how
    refined s alg (Interrupt a b x) = undefined
deriving instance Functor (GetCmd InterruptCmd k)
deriving instance Functor (CoCmd  InterruptCmd k)


instance HasDSL MacguffinCmd where
    data GetCmd MacguffinCmd all a = Macguffin (Desirable -> a) deriving Functor
    data CoCmd  MacguffinCmd all a = CoMacguffin
                                   { macguffinH :: (Desirable, a)
                                   } deriving Functor
    refined _ _ (Macguffin x) = Macguffin x


refine :: forall cmd cmds b.
          (cmd :<+: cmds, Functor (JoinedList cmds))
       => SStoryCmd cmd
       -> (forall cmds a. GetCmd cmd cmds a -> KnotStory (Filter ((:/=$) @@ cmd) cmds) a)
       -> KnotStory cmds b
       -> KnotStory (Filter ((:/=$) @@ cmd) cmds) b
refine s _   (Story (Pure x)) = Story $ Pure $ undefined
refine s alg (Story (Free x)) = Story $ Free $ go x
  where
    outj' = outj :: Summed (Functors cmds) c -> Maybe (GetCmd cmd cmds c)
    go (outj' -> Just (fa)) = undefined -- removing fa
    go other                = undefined -- keeping other

type MyCmds           = '[ChangeCmd, InterruptCmd, MacguffinCmd]
type Functors     k   = MapToFunctorList k k
type JoinedList   k   = Summed (Functors k)
newtype KnotStory k a = Story (Free (JoinedList k) a)
type cmd :<+: cmds    = GetCmd cmd cmds :<: Functors cmds

deriving instance Functor (JoinedList k) => Functor (KnotStory k)
deriving instance Functor (JoinedList k) => Applicative (KnotStory k)
deriving instance Functor (JoinedList k) => Monad (KnotStory k)

type KnotCoStory k = Cofree (CoKnot Product k)
type Story         = KnotStory   MyCmds
type CoStory       = KnotCoStory MyCmds

change :: forall cmds. (ChangeCmd :<+: cmds)
       => Character -> ChangeType -> KnotStory cmds ChangeResult
change c ct = Story . liftF $ inj (Change c ct id :: GetCmd ChangeCmd cmds ChangeResult)

interrupt :: forall cmds x y. (InterruptCmd :<+: cmds)
       => KnotStory (Filter ((:/=$) @@ 'InterruptCmd) cmds) x
       -> KnotStory (Filter ((:/=$) @@ 'InterruptCmd) cmds) y
       -> KnotStory cmds y
interrupt x y = Story . liftF $ inj (Interrupt x y id :: GetCmd InterruptCmd cmds y)

macguffin :: forall cmds. (MacguffinCmd :<+: cmds)
       => KnotStory cmds Desirable
macguffin = Story . liftF $ inj (Macguffin id :: GetCmd MacguffinCmd cmds Desirable)

test :: Story Bool
test = do
    change (Character "") Die
    x <- macguffin
    y <- interrupt (return ()) macguffin
    return $ x == y


injOutj_prop :: forall fs f a. (f :<: fs) => Proxy fs -> f a -> Bool
injOutj_prop _ fa = isJust $ (outj (inj fa :: Summed fs a) :: Maybe (f a))

main = quickCheck (injOutj_prop (Proxy @'[[], Proxy, Maybe, (,) Int]) :: Maybe Int -> Bool)

