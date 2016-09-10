{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Singletons.Prelude.List (Elem)



data Desirable = Desirable String deriving (Eq, Ord)
instance Show Desirable where
    show (Desirable name) = name

data Character = Character String deriving (Eq, Ord)
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
    data StoryCmd = ChangeCmd | InterruptCmd | MacguffinCmd deriving Show
    |])

type Knot c a = Knotted c a a

-- Fold a type-level list of functors into a right-associative data-type-a-la-carte.
type family Knotted (ctr :: (Type -> Type) -> (Type -> Type) -> Type -> Type)
                    -- ^The constructor for the a-la-cartedness.
                    (all :: [StoryCmd])
                    -- ^The entire type-level list.
                    (cont :: [StoryCmd]) :: Type -> Type
                    -- ^The thus-processed type-level list.
type instance Knotted ctr all (x ': '[]) = Tie all x
type instance Knotted ctr all (x ': (y ': ys)) = ctr (Tie all x) (Knotted ctr all (y ': ys))

-- Filters functors out of recursive calls of the fold.
type family Filter (xs :: [StoryCmd]) :: [StoryCmd] where
    Filter '[] = '[]
    Filter (x ': xs) = Keep x xs

-- Given a list of functors, return the functors in the opposite category.
type family CoList (all :: [StoryCmd]) (xs :: [StoryCmd]) :: [Type -> Type] where
    CoList all '[] = '[]
    CoList all (x ': xs) = Co all x ': CoList all xs

-- Every functor to be used in our Story DSL must have an instance of this class.
class HasDSL (t :: StoryCmd) where
    type GetCmd t :: Type -> Type

    -- Used to tie the recursive knot. Non-recursive functors should use the
    -- default implementation.
    type Tie (all :: [StoryCmd]) t :: Type -> Type
    type Tie all t = GetCmd t

    -- Used to detemrine whether this functor should be included in recursive
    -- instances. Non-recursive functors should use the default implementation.
    type Keep t (rest :: [StoryCmd]) :: [StoryCmd]
    type Keep k xs = k ': Filter xs

    -- Returns the handler for this term.
    type Co (all :: [StoryCmd]) t :: Type -> Type

data ChangeF a = Change Character ChangeType (ChangeResult -> a) deriving Functor
data CoChangeF a = CoChange
                 { changeH :: Character -> ChangeType -> (ChangeResult, a)
                 } deriving Functor
instance HasDSL ChangeCmd where
    type GetCmd ChangeCmd = ChangeF
    type Co all ChangeCmd = CoChangeF

data InterruptF k a = forall x y. Interrupt (Free k x) (Free k y) (y -> a)
instance Functor (InterruptF k) where
    fmap f (Interrupt x y a) = Interrupt x y (f . a)
data CoInterruptF k a = CoInterrupt
                      { interruptH :: forall x y. KnotStory k x -> KnotStory k y -> (y, a)
                      }
instance Functor (CoInterruptF k) where
    fmap f (CoInterrupt g) = CoInterrupt $ (fmap . fmap . fmap) f g
instance HasDSL InterruptCmd where
    type GetCmd InterruptCmd = InterruptF Any
    type Tie all InterruptCmd = InterruptF (Knot Sum (Filter all))
    type Keep InterruptCmd rest = rest
    type Co all InterruptCmd = CoInterruptF (Filter all)

data MacguffinF a = Macguffin (Desirable -> a) deriving Functor
data CoMacguffinF a = CoMacguffin
                    { macguffinH :: (Desirable, a)
                    } deriving Functor
instance HasDSL MacguffinCmd where
    type GetCmd MacguffinCmd = MacguffinF
    type Co all MacguffinCmd = CoMacguffinF

type KnotStory k = Free (Knot Sum k)
-- type KnotCoStory k = Cofree (Knot Product (CoList k k))
type MyCmds = '[ChangeCmd, InterruptCmd, MacguffinCmd]
type Story = KnotStory MyCmds
-- type CoStory = KnotCoStory MyCmds


change :: (MonadFree f m, ChangeF :<: f) => Character -> ChangeType -> m ChangeResult
change c ct = liftF . inj $ Change c ct id

magic :: Story ()
magic = do
    change (Character "") Die
    return ()

class ToTermLevel k (x :: [k]) where
    toTermLevel :: Proxy x -> [k]

instance ToTermLevel k '[] where
    toTermLevel _ = []

instance ( SingKind k
         , SingI x
         , ToTermLevel k xs
         , Demote x ~ k
         ) => ToTermLevel k ((x :: k) ': (xs :: [k])) where
    toTermLevel _ = fromSing (sing :: Sing x) : toTermLevel (Proxy @xs)

termLevel :: [StoryCmd]
termLevel = toTermLevel $ Proxy @MyCmds

-- proof :: (CoStory ~ Cofree (CoChangeF :*: CoInterruptF '[ChangeF, MacguffinF] :*: CoMacguffinF)) => ()
-- proof = ()


