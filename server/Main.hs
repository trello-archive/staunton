{-# LANGUAGE OverloadedStrings #-}
import           BasePrelude
import qualified Data.Map as M
import           Data.Text (Text)
import           Control.Concurrent
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

broadcast :: Text -> Text -> M.Map k WS.Connection -> IO ()
broadcast user msg toUsers = do
  let line = user <> ": " <> msg
  T.putStrLn line
  forM_ (M.elems toUsers) (\conn -> WS.sendTextData conn line)

newDB :: M.Map Text WS.Connection
newDB = M.empty

application
  :: Text -> WS.Connection -> MVar (M.Map Text WS.Connection) -> IO ()
application email conn db = do
  liftIO setup
  finally talk disconnect
  where
    setup = do
      T.putStrLn ("+ Setup: " <> email)
      modifyMVar_ db $ \users ->
        return (M.insert email conn users)
    disconnect = do
      T.putStrLn ("+ Disconnect: " <> email)
      modifyMVar_ db $ \users ->
        return (M.delete email users)
    talk = forever $ do
      T.putStrLn ("+ Broadcast: " <> email)
      msg <- WS.receiveData conn
      liftIO (readMVar db >>= broadcast email msg)

main :: IO ()
main = do
  db <- newMVar newDB
  T.putStrLn ("+ Listening to " <> ip <> ":" <> (T.pack $ show port))
  WS.runServer (T.unpack ip) port $ \pending -> do
    conn <- WS.acceptRequest pending
    email <- WS.receiveData conn
    application email conn db
  where
    ip = "0.0.0.0"
    port = 9160
