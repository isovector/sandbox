{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Codo where

import Data.Bool (bool)
import Language.Haskell.Codo
import Control.Comonad
import Control.Comonad.Store

data RowId = R1 | R2 | R3 | R4 | R5 deriving (Eq, Bounded, Enum)
data ColId = C1 | C2 | C3 | C4 | C5 deriving (Eq, Bounded, Enum)
data Coins = N1 | N2 | N3 | N4 | N5 deriving (Eq, Bounded, Enum)

type W = Store (RowId, ColId, Coins)

prev :: (Bounded a, Enum a, Eq a) => a -> [a]
prev a = bool [] (pure $ pred a) $ a /= minBound

next :: (Bounded a, Enum a, Eq a) => a -> [a]
next a = bool [] (pure $ succ a) $ a /= maxBound

getCoin :: [[Double]] -> RowId -> ColId -> Double
getCoin coins r c = (coins !! fromEnum c) !! fromEnum r

recurrence :: [[Double]]
           -> W (W Double)
           -> Double
recurrence coins = [codo| w =>
  let (d,s,r) = pos w
  let (d,s,r) = pos w
  maximum
      $ [ getCoin coins d s
        + peek (d', s, r') w
        | r' <- prev r
        , d' <- next d
        ]
     ++ [ peek (minBound, s', r) w
        | s' <- next s
        ]
  |]

coinage :: [[Double]]
        -> Int
        -> Store (RowId, ColId, Coins) Double
        -> Double
coinage coins num = [codo| w =>
  let (s,d,r) = pos w
  fromIntegral $ fromEnum r
  |]
