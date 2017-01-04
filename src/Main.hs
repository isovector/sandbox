{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.State (get, runState, modify)
import qualified Data.Foldable as F

import Idiom
import qualified Stories as Stories

loeb :: Traversable t
     => t a
     -> t (t a -> a)
loeb ta = fst
        . flip runState 0
        . flip mapM ta
        . const $ do
            here <- get
            modify (+1)
            return $ \ta' ->
                F.toList ta' !! here

main = putStrLn $ show test
