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
newtype Scoreboard = Scoreboard (Map Player Float)

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

instance A.ToJSON Scoreboard where
  toJSON (Scoreboard scores) =
    A.object [("scoreboard", A.toJSON tuples)]
    where
      tuples = [(email, score) | (Player email, score) <- M.toList scores]

read :: MVar a -> IO a
read = C.readMVar

modify :: MVar a -> (a -> IO a) -> IO ()
modify = C.modifyMVar_

ping :: WS.Connection -> IO ()
ping conn = WS.sendTextData conn ("{\"ping\": true}" :: Text)

heartbeat :: Int64 -> DB -> IO DB
heartbeat wait db =
  M.fromList <$> heartbeatFilterM (M.toList db)
  where
    delta =
      secondsToUnixDiffTime wait
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

broadcast :: (A.ToJSON a) => DB -> a -> IO ()
broadcast db obj =
  forM_ (M.toList db) unicast
  where
    unicast (_, (_, _, conn)) =
      WS.sendTextData conn (A.encode obj)

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

scoring :: Location -> DB -> Map Player Float -> IO (Map Player Float)
scoring hill db scores = do
  broadcast db (Scoreboard scores)
  return scores'
  where
    scores' =
      M.mapWithKey (\player (loc, _, _) -> score player + score' loc) db
    score player =
      maybe 0 id (M.lookup player scores)
    score' loc =
      if distance loc > 0.01 then 1 / (distance loc) else 100
    distance loc =
      let (x0, y0) = loc
          (x1, y1) = hill in
      sqrt ((x1 - x0) ^ two + (y1 - y0) ^ two)
    two = 2 :: Int -- :(

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
  kingState <- C.newMVar =<< king
  kingTimer <- makeTimer (modify kingState $ const king) secondsPerKing
  putStrLn "+ King up"

  heartbeatTimer <-
    makeTimer (modify state $ heartbeat maxSecondsBeforeGC) secondsPerHeartbeat
  putStrLn "+ Heartbeat up"

  let broadcastIO =
        join (broadcast <$> read state <*> (World <$> read state <*> read kingState))
  broadcastTimer <-
    makeTimer broadcastIO secondsPerBroadcast
  putStrLn "+ Broadcast up"

  scoringState <- C.newMVar M.empty
  let scoringIO scores =
        join (scoring <$> read kingState <*> read state <*> pure scores)
  scoringTimer <-
    makeTimer (modify scoringState scoringIO) secondsPerScoring
  putStrLn "+ Scoring up"

  let timers = [ heartbeatTimer
               , broadcastTimer
               , kingTimer
               , scoringTimer
               ]
  (`finally` (forM_ timers stopTimer)) application

  where
    makeTimer io secs = repeatedTimer io (sDelay secs)

    secondsPerHeartbeat = 20
    secondsPerBroadcast = 1
    secondsPerKing = 5
    secondsPerScoring = 1
    maxSecondsBeforeGC = secondsPerHeartbeat * 2


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
