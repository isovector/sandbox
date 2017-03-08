{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Freer where

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer

countWrites :: forall r a
             . Member IO r
            => Eff (Writer () ': r) a
            -> Eff r a
countWrites = fmap fst
            . flip runState (0 :: Int)
            . replaceRelay pure bind
  where
    bind :: forall x
          . Writer () x
         -> (x -> Eff (State Int ': r) a)
         -> Eff (State Int ': r) a
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

