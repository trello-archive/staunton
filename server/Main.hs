{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, NamedFieldPuns #-}

import           BasePrelude hiding ((\\), finally)
import           Control.Concurrent (MVar)
import qualified Control.Concurrent as C
import           Control.Concurrent.Suspend (sDelay)
import           Control.Concurrent.Timer (repeatedTimer, stopTimer)
import           Control.Monad.Catch (finally)
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson as A
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.UnixTime (UnixTime, getUnixTime, secondsToUnixDiffTime, diffUnixTime)
import qualified Network.WebSockets as WS
import           System.IO
import           System.IO.Streams.Attoparsec (ParseException)

type Email = Text
type Location = (Float, Float)
data Player = Player Email deriving (Eq, Ord, Show)
data Move = Move Location deriving (Show)
data Pong = Pong Bool
type DB = Map Player (Location, UnixTime, WS.Connection)

instance A.FromJSON Player where
  parseJSON (A.Object o) = Player <$> o A..: "email"

instance A.FromJSON Move where
  parseJSON (A.Object o) = Move <$> ((,) <$> o A..: "x" <*> o A..: "y")

instance A.FromJSON Pong where
  parseJSON (A.Object o) = Pong <$> o A..: "pong"

ping :: WS.Connection -> IO ()
ping conn = WS.sendTextData conn ("{\"ping\": true}" :: Text)

heartbeatIntervalSeconds :: Int64
heartbeatIntervalSeconds = 1

heartbeat :: DB -> IO DB
heartbeat db =
  M.fromList <$> heartbeatFilterM (M.toList db)
  where
    delta =
      secondsToUnixDiffTime (heartbeatIntervalSeconds * 2)
    heartbeatFilterM =
      filterM $ \(player, (_, lastPongTime, conn)) -> do
        now <- getUnixTime
        case (diffUnixTime now lastPongTime > delta) of
         True -> do
           putStrLn ("+ GCing " <> show player)
           WS.sendClose conn ("pong better" :: Text)
           return False
         False -> do
           ping conn
           return True

dataflow :: (Player
             -> WS.Connection
             -> IO (Move -> IO (), Pong -> IO (), IO ()))
            -> WS.PendingConnection -> IO ()
dataflow onConnect pending = do
  conn <- WS.acceptRequest pending
  initial <- WS.receiveData conn
  case A.decode initial of
   Nothing -> do
     putStrLn ("Invalid registration: " <> show initial)
   Just player -> do
     (onMove, onPong, onDisconnect) <- onConnect player conn
     ((`finally` onDisconnect) . runMaybeT . forever) $ do
       message <- lift $ WS.receiveData conn
       case A.decode message of
        Just move ->
          lift $ onMove move
        Nothing -> do
          case A.decode message of
           Just pong ->
             lift $ onPong pong
           Nothing -> do
             lift $ putStrLn ("Unrecognized: " <> show message)
             unforever
     putStrLn "+ Finally over"
     return ()
  where
    -- forever in the IO monad loops forever, as you might suspect,
    -- without giving us a way to break out. forever in the
    -- MaybeT IO monad, however, is quite delightful.
    unforever = mzero

mainWithState :: MVar DB -> IO ()
mainWithState state = do
  putStrLn "+ Heartbeat up"
  timer <- repeatedTimer (withDB $ heartbeat) (sDelay heartbeatIntervalSeconds)
  (`finally` (stopTimer timer)) $ runServer (dataflow application)
  where
    withDB =
      C.modifyMVar_ state
    application player conn = do
      putStrLn ("+ Connecting " <> show player)
      renew
      return (onMove, onPong, onDisconnect)
      where
        renew = withDB $ \db -> do
          now <- getUnixTime
          return (M.insert player ((0, 0), now, conn) db)
        onMove (Move to) = withDB $ \db -> do
          putStrLn ("+ Move from " <> show player <> ": " <> show to)
          now <- getUnixTime
          return (M.insert player (to, now, conn) db)
        onDisconnect = withDB $ \db -> do
          putStrLn ("+ Disconnecting " <> show player)
          return (M.delete player db)
        onPong _ = do
          renew

runServer :: (WS.PendingConnection -> IO ()) -> IO ()
runServer server = do
  putStrLn ("+ Server up @" <> ip <> ":" <> show port)
  WS.runServer ip port (handle connectionExceptions .
                        handle parseExceptions .
                        server)
  where
    ip = "0.0.0.0"
    port = 9160

    connectionExceptions :: WS.ConnectionException -> IO ()
    connectionExceptions _ =
      -- Our finally handler is sufficient.
      return ()

    parseExceptions :: ParseException -> IO ()
    parseExceptions _ = do
      throw WS.ConnectionClosed

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  state <- C.newMVar (M.empty)
  mainWithState state
