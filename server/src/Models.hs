module Models where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.UnixTime (UnixTime, getUnixTime)

import BasePrelude

data Player =
  Player { playerEmail :: Text }
  deriving (Eq, Ord, Show)

type Location =
  (Float, Float)

data State =
  State Player Location UnixTime

data Message =
    MessageConnect Player Location
  | MessageMove Location
  | MessagePong
  | MessageDisconnect

data Registration =
  Registration Player Location deriving (Show)

registrationOfBytes :: ByteString -> Maybe Registration
registrationOfBytes = undefined

stateOfRegistration :: Registration -> IO State
stateOfRegistration (Registration player location) = do
  now <- getUnixTime
  return (State player location now)

messageOfBytes :: ByteString -> Maybe Message
messageOfBytes = undefined
