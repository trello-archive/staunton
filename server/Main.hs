{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, NamedFieldPuns #-}

import           BasePrelude hiding ((\\), finally, read)
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
import           System.Random (getStdRandom, randomR)

type Email = Text
type Location = (Float, Float)
data Player = Player Email deriving (Eq, Ord, Show)
data Move = Move Location deriving (Show)
data Pong = Pong Bool
type DB = Map Player (Location, UnixTime, WS.Connection)
data World = World DB Location

instance A.FromJSON Player where
  parseJSON (A.Object o) = Player <$> o A..: "email"
  parseJSON _ = error "Invalid player"

instance A.FromJSON Move where
  parseJSON (A.Object o) = Move <$> ((,) <$> o A..: "x" <*> o A..: "y")
  parseJSON _ = error "Invalid move"

instance A.FromJSON Pong where
  parseJSON (A.Object o) = Pong <$> o A..: "pong"
  parseJSON _ = error "Invalid pong"

instance A.ToJSON World where
  toJSON (World db kingLocation) =
    A.object [("world", A.toJSON tuples)]
    where
      tuples =
        [(email, loc) | (Player email, (loc, _, _)) <- M.toList db] <>
        [("king@aol.com", kingLocation)]

read :: MVar a -> IO a
read = C.readMVar

modify :: MVar a -> (a -> IO a) -> IO ()
modify = C.modifyMVar_

ping :: WS.Connection -> IO ()
ping conn = WS.sendTextData conn ("{\"ping\": true}" :: Text)

secondsPerBroadcast :: Int64
secondsPerBroadcast = 1

secondsPerHeartbeat :: Int64
secondsPerHeartbeat = 20

secondsPerKing :: Int64
secondsPerKing = 5

heartbeat :: DB -> IO DB
heartbeat db =
  M.fromList <$> heartbeatFilterM (M.toList db)
  where
    delta =
      secondsToUnixDiffTime (secondsPerHeartbeat * 2)
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

broadcast :: DB -> Location -> IO ()
broadcast db kingLocation =
  forM_ (M.toList db) unicast
  where
    json =
      A.encode (World db kingLocation)
    unicast (_, (_, _, conn)) =
      WS.sendTextData conn json

king :: IO Location
king = do
  x <- randomFloat
  y <- randomFloat
  let location = (x, y) :: Location
  putStrLn ("+ King moving to: " <> show location)
  return (x, y)
  where
    randomFloat = ((/ 1024) . fromIntegral) <$> getStdRandom limits
    limits = randomR (0, 1024 :: Int)

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
     (void . (`finally` onDisconnect) . runMaybeT . forever) $ do
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

runTimers :: MVar DB -> IO () -> IO ()
runTimers state application = do
  kingState <- C.newMVar (0, 0)
  kingTimer <- makeTimer (C.modifyMVar_ kingState $ const king) secondsPerKing
  putStrLn "+ King up"
  heartbeatTimer <- makeTimer (modify state $ heartbeat) secondsPerHeartbeat
  putStrLn "+ Heartbeat up"
  let kingIO = join (broadcast <$> read state <*> read kingState)
  broadcastTimer <- makeTimer kingIO secondsPerBroadcast
  putStrLn "+ Broadcast up"
  (`finally` (forM_ [heartbeatTimer, broadcastTimer, kingTimer] stopTimer)) application
  where
    makeTimer io secs = repeatedTimer io (sDelay secs)

mainWithState :: MVar DB -> IO ()
mainWithState state = do
  runTimers state $ runServer (dataflow application)
  where
    application player conn = do
      putStrLn ("+ Connecting " <> show player)
      renew
      return (onMove, onPong, onDisconnect)
      where
        renew = modify state $ \db -> do
          now <- getUnixTime
          return (M.insert player ((0, 0), now, conn) db)
        onMove (Move to) = modify state $ \db -> do
          putStrLn ("+ Move from " <> show player <> ": " <> show to)
          now <- getUnixTime
          return (M.insert player (to, now, conn) db)
        onDisconnect = modify state $ \db -> do
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
