{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Proxy (Proxy (..))
import Graph

type Time = Double

data Metadata a = Metadata
                { machineData :: a
                , totalTime   :: Time
                , stateTime   :: Time
                }

class HasMachine state where
    type MachineData state :: *

    start :: Proxy state
          -> (state, MachineData state)

    shouldTerminate :: state
                    -> Metadata (MachineData state)
                    -> Bool

    pump :: state
         -> Metadata (MachineData state)
         -> (state, MachineData state)


data Machine where
    Machine :: HasMachine m => (m, MachineData m) -> Machine

newMachine :: HasMachine m => Proxy m -> Machine
newMachine = Machine . start

pumpMachine :: Time -> Time -> Machine -> Maybe Machine
pumpMachine tt st (Machine (s, md)) =
    let metadata = Metadata md tt st
     in if shouldTerminate s metadata
           then Nothing
           else Just . Machine $ pump s metadata


-------------------------------------------------


data Fireball = Animate | Mash | Payload

instance HasMachine Fireball where
    type MachineData Fireball = Int

    shouldTerminate Payload (Metadata 0 _ _) = True
    shouldTerminate _ _                      = False

    start _ = (Animate, 0)

    pump Animate (stateTime -> st)
        | st >= 1   = (Mash, 0)
        | otherwise = (Animate, 0)

    pump Mash md
        | stateTime md >= 3 = (Payload, machineData md)
        | otherwise         = (Mash, machineData md + 1)

    pump Payload (machineData -> i) = (Payload, i - 1)


-------------------------------------------------


main = putStrLn "hello"
