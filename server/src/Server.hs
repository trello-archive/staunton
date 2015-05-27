import           Control.Concurrent (MVar)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import           Data.Aeson (decode)
import           Data.Map (Map)
import           Machine (tick)
import           System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))

import qualified Control.Concurrent as Conc
import qualified Data.Map as Map
import qualified Network.WebSockets as WS

import           BasePrelude
import           Models

-- Everything we know.
data Server =
  Server (MVar (Map Player State)) String Int

traverseConn :: WS.Connection -> (Message -> MaybeT IO a) -> IO ()
traverseConn conn f =
  (void . runMaybeT . forever) $ do
    bytes <- lift (WS.receiveData conn)
    message <- MaybeT . return $ messageOfBytes bytes
    f message

push :: a -> Maybe (a, b) -> (a, Maybe b)
push a mab = case mab of
  Nothing -> (a, Nothing)
  Just (a', b) -> (a', Just b)

modifyMVarM :: MVar a -> (a -> MaybeT IO (a, b)) -> MaybeT IO b
modifyMVarM mvar a2mab =
  MaybeT (Conc.modifyMVar mvar ((fmap <$> push) <*> runMaybeT . a2mab))

authenticatedIOOfConnection :: Server -> Player -> WS.Connection -> IO ()
authenticatedIOOfConnection (Server db _ _) player conn =
  traverseConn conn (\message ->
                      modifyMVarM db $ \states -> do
                        state <- MaybeT . return $ Map.lookup player states
                        state' <- tick state message
                        return (Map.insert player state' states, state'))

ioOfConnection :: Server -> WS.PendingConnection -> IO ()
ioOfConnection server@(Server db _ _) pending = do
  conn <- WS.acceptRequest pending
  bytes <- WS.receiveData conn
  case registrationOfBytes bytes of
   Nothing ->
     putStrLn ("Invalid registration: " <> show bytes)
   Just reg@(Registration player _) -> do
     state0 <- stateOfRegistration reg
     Conc.modifyMVar_ db (return . Map.insert player state0)
     putStrLn ("New player: " <> show reg)
     authenticatedIOOfConnection server player conn

newServer :: IO Server
newServer = do
  db <- Conc.newMVar Map.empty
  return (Server db "0.0.0.0" 9160)

runServer :: Server -> IO ()
runServer server@(Server _ ip port) = do
  putStrLn ("+ Server up on " <> ip <> ":" <> show port)
  WS.runServer ip port (ioOfConnection server)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  newServer >>= runServer
