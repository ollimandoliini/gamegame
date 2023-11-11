{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar, threadDelay)
import Control.Exception (finally)
import Control.Monad (forM_, forever, void)
import Data.Aeson (encode, decode, eitherDecode)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Lib (Message (..), UserMsg (..))
import Network.HTTP.Types (status400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS

type Client = (Text, WS.Connection)
type ServerState = [Client]

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Message -> ServerState -> IO ()
broadcast message clients = forM_ clients $ \(_, conn) -> WS.sendBinaryData conn (encode message)

application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
        -- msg <- WS.receiveData conn
        -- clients <- readMVar state
        putStrLn "New client connected"
        let newClient = ("user", conn)
        flip finally (disconnect newClient) $ do
            void $ modifyMVar_ state $ \s -> pure (newClient : s)
            sendMsg Connected conn
            receiver conn state
  where
    disconnect client = do
        s <- modifyMVar state $ \s ->
            let s' = removeClient client s
             in return (s', s')
        T.putStrLn $ fst client <> " disconnected"
        -- broadcast (fst client <> " disconnected") s

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



-- talk :: Client -> MVar ServerState -> IO ()
-- talk (user, conn) state = forever $ do
--     msg <- WS.receiveData conn
--     readMVar state
--         >>= broadcast
--             (user `mappend` ": " `mappend` msg)

app :: MVar ServerState -> Application
app state = do
    websocketsOr WS.defaultConnectionOptions (application state) backupApp
  where
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

main :: IO ()
main = do
    let port = 3000
    T.putStrLn $ "Running server on port " <> T.pack (show port) <> "..."
    state <- newMVar []
    run port $ app state
