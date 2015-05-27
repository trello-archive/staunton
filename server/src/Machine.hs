module Machine
       ( tick
       ) where

import qualified Data.Map as Map

import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans (lift)
import Data.UnixTime (getUnixTime)

import BasePrelude
import Models

tick :: State -> Message -> MaybeT IO State
tick (State player location _) transition =
  case transition of
   MessagePong -> do
     now <- lift getUnixTime
     return (State player location now)
   MessageMove loc -> do
     now <- lift getUnixTime
     return (State player loc now)
   MessageDisconnect ->
     mzero
