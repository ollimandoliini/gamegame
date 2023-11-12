{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, void)
import Data.Aeson (eitherDecode, encode)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as T
import Lib (Message (..), UserMsg (..))
import Network.HTTP.Types (status400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
  ( PendingConnection (pendingRequest)
  , RequestHead (requestPath)
  )
import Network.WebSockets qualified as WS

type Client = (Text, WS.Connection)
type ServerState = [Client]

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Message -> ServerState -> IO ()
broadcast message clients = forM_ clients $
  \(_, conn) -> WS.sendBinaryData conn (encode message)

application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do
  conn <- WS.acceptRequest pending
  case T.drop 1 . decodeUtf8 . requestPath . pendingRequest $ pending of
    username -> do
      WS.withPingThread conn 30 (return ()) $ do
        T.putStrLn $ "New client '" <> username <> "' connected"
        let newClient = (username, conn)
        flip finally (disconnect newClient) $ do
          void $ modifyMVar_ state $ \s -> pure (newClient : s)
          sendMsg Connected conn
          receiver conn state
  where
    disconnect client = do
      modifyMVar_ state $ \s -> return (removeClient client s)
      T.putStrLn $ fst client <> " disconnected"

sendMsg :: Message -> WS.Connection -> IO ()
sendMsg msg conn = WS.sendTextData conn (encode msg)

receiver :: WS.Connection -> MVar ServerState -> IO ()
receiver conn state = do
  msg <- WS.receiveData conn
  s <- readMVar state
  print msg
  case eitherDecode msg of
    Right msg' -> broadcast (UserMessage msg') s >> receiver conn state
    Left err -> print err >> receiver conn state

app :: MVar ServerState -> Application
app state = do
  websocketsOr WS.defaultConnectionOptions (application state) backupApp
  where
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

main :: IO ()
main = do
  let port = 8080
  T.putStrLn $ "Running server on port " <> T.pack (show port) <> "..."
  state <- newMVar []
  run port $ app state
