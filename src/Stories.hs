{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Stories
     where

import Control.Monad.Free
import Control.Comonad.Cofree

data Placeholder :: * -> *




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


type Knot c a = Knotted c a a

-- Fold a type-level list of functors into a right-associative data-type-a-la-carte.
type family Knotted (ctr :: (* -> *) -> (* -> *) -> * -> *)
                    -- ^The constructor for the a-la-cartedness.
                    (all :: [* -> *])
                    -- ^The entire type-level list.
                    (cont :: [* -> *]) :: * -> *
                    -- ^The thus-processed type-level list.
type instance Knotted ctr all (x ': '[]) = Tie all x
type instance Knotted ctr all (x ': (y ': ys)) = ctr (Tie all x) (Knotted ctr all (y ': ys))

-- Filters functors out of recursive calls of the fold.
type family Filter (xs :: [* -> *]) where
    Filter '[] = '[]
    Filter (x ': xs) = Keep x xs

-- Given a list of functors, return the functors in the opposite category.
type family CoList (all :: [* -> *]) (xs :: [* -> *]) where
    CoList all '[] = '[]
    CoList all (x ': xs) = Co all x ': CoList all xs

-- Every functor to be used in our Story DSL must have an instance of this class.
class HasDSL (t :: * -> *) where
    -- Used to tie the recursive knot. Non-recursive functors should use the
    -- default implementation.
    type Tie (all :: [* -> *]) t :: * -> *
    type Tie all t = t

    -- Used to detemrine whether this functor should be included in recursive
    -- instances. Non-recursive functors should use the default implementation.
    type Keep t (rest :: [* -> *]) :: [* -> *]
    type Keep k xs = k ': Filter xs

    -- Returns the handler for this term.
    type Co (all :: [* -> *]) t :: * -> *

data ChangeF a = Change Character ChangeType (ChangeResult -> a) deriving Functor
instance HasDSL ChangeF where
    type Co all ChangeF = CoChangeF

data InterruptF k a = forall x y. Interrupt (Free k x) (Free k y) (y -> a)
instance Functor k => Functor (InterruptF k) where
    fmap f (Interrupt x y a) = Interrupt x y (f . a)
instance HasDSL (InterruptF k) where
    type Tie all (InterruptF k) = InterruptF (Knot Sum (Filter all))
    type Keep (InterruptF k) rest = rest
    type Co all (InterruptF k) = CoInterruptF (Filter all)

data MacguffinF a = Macguffin (Desirable -> a) deriving Functor
instance HasDSL MacguffinF where
    type Co all MacguffinF = CoMacguffinF



type KnotStory k = Free (Knot Sum k)
type KnotCoStory k = Cofree (Knot Product (CoList k k))
type Story = KnotStory '[ChangeF, InterruptF Placeholder, MacguffinF]
type CoStory = KnotCoStory '[ChangeF, InterruptF Placeholder, MacguffinF]

change :: (MonadFree f m, ChangeF :<: f) => Character -> ChangeType -> m ChangeResult
change c ct = liftF . inj $ Change c ct id

magic :: Story ()
magic = do
    change (Character "") Die
    return ()

proof :: (CoStory ~ Cofree (CoChangeF :*: CoInterruptF '[ChangeF, MacguffinF] :*: CoMacguffinF)) => ()
proof = ()



data CoChangeF a = CoChange
                 { changeH :: Character -> ChangeType -> (ChangeResult, a)
                 }
data CoInterruptF k a = CoInterrupt
                      { interruptH :: forall x y. KnotStory k x -> KnotStory k y -> (y, a)
                      }
data CoMacguffinF a = CoMacguffin
                    { macguffinH :: (Desirable, a)
                    }

