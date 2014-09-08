{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, NamedFieldPuns #-}

import           BasePrelude hiding ((\\), finally, read)
import           Control.Concurrent (MVar)
import qualified Control.Concurrent as C
import           Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Ch
import           Control.Concurrent.Suspend (sDelay)
import           Control.Concurrent.Timer (repeatedTimer, stopTimer, TimerIO)
import           Control.Monad.Catch (finally)
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson as A
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UnixTime (UnixTime, getUnixTime, secondsToUnixDiffTime, diffUnixTime)
import           Network.WebSockets (Connection)
import qualified Network.WebSockets as WS
import           System.IO
import           System.IO.Streams.Attoparsec (ParseException)
import           System.Random (getStdRandom, randomR)

type Email = Text
type Location = (Float, Float)
data Player = Player Email deriving (Eq, Ord, Show)
data Move = Move Location deriving (Show)
data Pong = Pong Bool
type DB = Map Player (Location, UnixTime, Connection)
data World = World DB
newtype Scoreboard = Scoreboard (Map Player Float)
newtype King = King Location
data Registration = Registration Player Location deriving (Show)

instance A.FromJSON Registration where
  parseJSON (A.Object o) = Registration <$> (Player <$> o A..: "email") <*> ((,) <$> o A..: "x" <*> o A..: "y")
  parseJSON _ = error "Invalid player"

instance A.FromJSON Move where
  parseJSON (A.Object o) = Move <$> ((,) <$> o A..: "x" <*> o A..: "y")
  parseJSON _ = error "Invalid move"

instance A.FromJSON Pong where
  parseJSON (A.Object o) = Pong <$> o A..: "pong"
  parseJSON _ = error "Invalid pong"

instance A.ToJSON World where
  toJSON (World db) =
    A.object [("world", A.toJSON tuples)]
    where
      tuples =
        [(email, loc) | (Player email, (loc, _, _)) <- M.toList db]

instance A.ToJSON King where
  toJSON (King loc) =
    A.object [("king", A.toJSON loc)]

instance A.ToJSON Scoreboard where
  toJSON (Scoreboard scores) =
    A.object [("scoreboard", A.toJSON tuples)]
    where
      tuples = [(email, score) | (Player email, score) <- M.toList scores]

read :: MVar a -> IO a
read = C.readMVar

modify :: MVar a -> (a -> IO a) -> IO a
modify m f = C.modifyMVar m $ \x -> do
  y <- f x
  return (y, y)

ping :: Connection -> IO ()
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

king :: DB -> IO Location
king db = do
  x <- randomFloat
  y <- randomFloat
  let location = (x, y) :: Location
  putStrLn ("+ King moving to: " <> show location)
  broadcast db (King location)
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

dataflow :: (Registration
             -> Connection
             -> IO (Move -> IO (), Pong -> IO (), IO ()))
            -> WS.PendingConnection -> IO ()
dataflow onConnect pending = do
  conn <- WS.acceptRequest pending
  initial <- WS.receiveData conn
  case A.decode initial of
   Nothing -> do
     putStrLn ("Invalid registration: " <> show initial)
   Just registration -> do
     (onMove, onPong, onDisconnect) <- onConnect registration conn
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

makeTimers :: MVar DB ->
              Chan (Player, Connection) ->
              Chan (Player, Connection) ->
              IO [TimerIO]
makeTimers state didConnect didMove = do
  king0 <- read state >>= king
  kingState <- C.newMVar king0
  let kingIO =
        join (king <$> read state)
  kingTimer <-
    makeTimer (void . modify kingState $ const kingIO) secondsPerKing
  _ <- forkIO $ do
    connects <- Ch.getChanContents didConnect
    (`mapM_` connects) $ \(_, conn) -> do
      loc <- read kingState
      WS.sendTextData conn (A.encode (King loc))
  putStrLn "+ King up"

  heartbeatTimer <-
    makeTimer (void . modify state $ heartbeat maxSecondsBeforeGC) secondsPerHeartbeat
  putStrLn "+ Heartbeat up"

  _ <- forkIO $ do
    moves <- Ch.getChanContents didMove
    (`mapM_` moves) $ \(_, _) -> do
      db <- read state
      broadcast db (World db)
  putStrLn "+ Broadcast up"

  scoringState <- C.newMVar M.empty
  let scoringIO scores =
        join (scoring <$> read kingState <*> read state <*> pure scores)
  scoringTimer <-
    makeTimer (void (modify scoringState scoringIO)) secondsPerScoring
  putStrLn "+ Scoring up"

  return [ heartbeatTimer
         , kingTimer
         , scoringTimer
         ]
  where
    makeTimer io secs = repeatedTimer io (sDelay secs)

    secondsPerHeartbeat = 20
    secondsPerKing = 5
    secondsPerScoring = 1
    maxSecondsBeforeGC = secondsPerHeartbeat * 2


mainWithState :: MVar DB ->
                 Chan (Player, Connection) ->
                 Chan (Player, Connection) ->
                 IO ()
mainWithState state didConnect didMove = do
  timers <- makeTimers state didConnect didMove
  (`finally` (forM_ timers stopTimer)) server
  where
    server =
      runServer (dataflow application)
    application registration@(Registration player home)  conn = do
      putStrLn ("+ Connecting " <> show registration)
      onConnect
      Ch.writeChan didConnect (player, conn)
      return (onMove, onPong, void onDisconnect)
      where
        drug now (Just (loc, _, _)) =
          Just (loc, now, conn)
        drug now (Nothing) =
          Just (home, now, conn)
        renew = modify state $ \db -> do
          now <- getUnixTime
          return (M.alter (drug now) player db)
        onConnect = do
          read state >>= \db -> do
            case (player, M.lookup player db) of
             (Player email, Just _) -> error ("This email address is already taken: " <> T.unpack email)
             (_, Nothing) -> return ()
            db' <- renew
            WS.sendTextData conn (A.encode (World db'))
        onMove (Move to) = do
          _ <- modify state $ \db -> do
            putStrLn ("+ Move from " <> show player <> ": " <> show to)
            now <- getUnixTime
            return (M.insert player (to, now, conn) db)
          Ch.writeChan didMove (player, conn)
        onDisconnect = modify state $ \db -> do
          putStrLn ("+ Disconnecting " <> show player)
          let db' = M.delete player db
          Ch.writeChan didMove (player, conn)
          return db'
        onPong _ = do
          void renew

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
  didConnect <- Ch.newChan
  didMove <- Ch.newChan
  mainWithState state didConnect didMove
