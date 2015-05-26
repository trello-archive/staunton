{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Client where

import           BasePrelude hiding (catch)
import           Control.Monad.Catch
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import           System.IO
import           Text.Printf

application :: Bool -> WS.ClientApp ()
application shouldPong conn = do
   (pingPongThread, send) <- makePingPongThread
   send "{\"email\": \"%s\"}"
   send "{\"player\": {\"email\": \"%s\"}, \"x\": 1.0, \"y\": 1.0}"
   send "{\"player\": {\"email\": \"%s\"}, \"x\": 0.0, \"y\": 0.0}"
   threadDelay $ 5 * 10 ^ 6
   send "{\"player\": {\"email\": \"%s\"}, \"x\": 0.5, \"y\": 0.5}"
   send "{\"player\": {\"email\": \"%s\"}, \"x\": 0.7, \"y\": 0.7}"
   killThread pingPongThread
   putStrLn "+ Good night"
  where
    makeSend :: ThreadId -> String -> IO ()
    makeSend pptid t = do
      let t' = printf t (show pptid)
      putStrLn ("+ Sending " <> t')
      WS.sendTextData conn (T.pack t')
    makePingPongThread = do
      thread <- forkIO $ (`catch` net) . forever $ do
        msg <- WS.receiveData conn
        putStrLn ("+ Pinged: " <> show (msg :: T.Text))
        case shouldPong of
         True -> WS.sendTextData conn ("{\"pong\": true}" :: T.Text)
        putStrLn ("+ Ponged")
      return (thread, makeSend thread)
    net ThreadKilled =
      return ()

client :: Bool -> IO ()
client shouldPong = do
  hSetBuffering stdout LineBuffering
  WS.runClient "localhost" 9160 "/" (application shouldPong)
