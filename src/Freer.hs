{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Freer where

import Data.Constraint
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Data.Open.Union
import Data.Open.Union.Internal
import Control.Monad.Freer.Writer
import Control.Monad.Freer.State
import Unsafe.Coerce
import Data.FTCQueue

inTermsOf :: forall s r x. (Member s r => x) -> x
inTermsOf wat = case unsafeCoerce (Dict :: Dict (Show ())) :: Dict (Member s r) of
                  Dict -> wat

raise :: (r ~ (Head r ': Tail r)) => Eff r a -> Eff (s ': r) a
raise (Val a) = Val a
raise (E u ftc) = E (weaken u) $ raiseFTC ftc

raiseFTC :: (r ~ (Head r ': Tail r)) => FTCQueue (Eff r) a b -> FTCQueue (Eff (s ': r)) a b
raiseFTC (Leaf f) = Leaf $ fmap raise f
raiseFTC (Node a b) = Node (raiseFTC a) (raiseFTC b)

countWrites :: forall r a. Member IO r
            => Eff (Writer () ': r) a
            -> Eff r a
countWrites prog = fmap fst
                 . flip runState (0 :: Int)
                 . raise
                 $ handleRelay pure
                               (inTermsOf @(State Int) @r bind)
                               prog
  where
    bind :: forall x. (Member (State Int) r) => Writer () x -> (x -> Eff r a) -> Eff r a
    bind (Writer _) arr = do
      x :: Int <- get
      send $ print x
      put $ x + 1
      arr ()

prog :: Eff '[Writer (), IO] ()
prog = do
  tell ()
  tell ()
  tell ()

main :: IO ()
main = runM $ countWrites prog

