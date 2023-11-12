{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Brick
  ( App (..)
  , BrickEvent (AppEvent, VtyEvent)
  , EventM
  , ViewportScroll (vScrollToEnd)
  , ViewportType (..)
  , Widget (..)
  , customMain
  , hLimit
  , modify
  , vLimit
  , viewport
  , viewportScroll
  , (<+>)
  , (<=>)
  )
import Brick.AttrMap qualified as A
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Brick.Main qualified as M
import Brick.Types (zoom)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Border.Style qualified as BS
import Brick.Widgets.Center (center)
import Brick.Widgets.Core
  ( str
  , txt
  , vBox
  , withBorderStyle
  )
import Brick.Widgets.Edit (Editor, applyEdit, handleEditorEvent, renderEditor)
import Brick.Widgets.Edit qualified as E
import Control.Concurrent (MVar, newEmptyMVar, readMVar)
import Control.Concurrent.Async (concurrently_, race_)
import Control.Concurrent.MVar (putMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import Data.Text (Text, unpack)
import Data.Text.Zipper (clearZipper)
import Graphics.Vty (defaultConfig)
import Graphics.Vty.Attributes qualified as V
import Graphics.Vty.Input.Events qualified as V
import Graphics.Vty.Platform.Unix (mkVty)
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (%=), (.=))
import Lens.Micro.TH (makeLenses)
import Lib (Message (..), UserMsg (..), content, sender)
import Network.WebSockets (receiveData, runClient, sendBinaryData)
import Network.WebSockets.Client (ClientApp)
import Network.WebSockets.Connection (Connection)

-- data Msg = Msg
--     { _content :: Text
--     , _sender :: Text
--     , _time :: UTCTime
--     }

data Event
  = ConnectionReadyEvent
  | MessageEvent UserMsg
  deriving (Show)

newtype Username = Username Text

data UIState
  = UsernameState
  | ConnectingState
  | ChatState Username [UserMsg]

data AppState = AppState
  { _eventChan :: BChan Event
  , _sendChan :: BChan UserMsg
  , _userNameMVar :: MVar Text
  , _msgEditor :: Editor Text Text
  , _userNameEditor :: Editor Text Text
  , _uiState :: UIState
  -- , _userName :: Maybe Text
  -- , _messages :: [Msg]
  }

makeLenses ''AppState

handleEvent :: BrickEvent Text Event -> EventM Text AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = M.halt
handleEvent event =
  use uiState >>= \case
    UsernameState -> handleUserNameEvent event
    ConnectingState -> handleConnectingEvent event
    ChatState msgs username -> handleChatEvent event -- TODO: Fix unused variables

handleUserNameEvent :: BrickEvent Text Event -> EventM Text AppState ()
handleUserNameEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  editorContent <- mconcat . E.getEditContents <$> use userNameEditor
  case editorContent of
    "" -> pure ()
    name -> connect name
handleUserNameEvent e = zoom userNameEditor $ handleEditorEvent e

connect :: Text -> EventM Text AppState ()
connect username = do
  uiState .= ConnectingState
  usernameMVar' <- use userNameMVar
  liftIO $ putMVar usernameMVar' username
  userNameEditor %= applyEdit clearZipper

handleConnectingEvent :: BrickEvent Text Event -> EventM Text AppState ()
handleConnectingEvent (AppEvent ConnectionReadyEvent) =
  uiState .= ChatState (Username "böö") []
handleConnectingEvent _ = pure ()

handleChatEvent :: BrickEvent Text Event -> EventM Text AppState ()
handleChatEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  newMsg <- mconcat . E.getEditContents <$> use msgEditor
  userNameMVar' <- use userNameMVar
  userName <- liftIO $ readMVar userNameMVar'
  sendChan' <- use sendChan
  liftIO $ writeBChan sendChan' (UserMsg newMsg userName)
  msgEditor %= applyEdit clearZipper
handleChatEvent (AppEvent (MessageEvent newMsg)) = do
  modify $ addMsg newMsg
  vScrollToEnd chatViewport
handleChatEvent e = zoom msgEditor $ handleEditorEvent e

addMsg :: UserMsg -> AppState -> AppState
addMsg msg = \case
  s@(AppState _ _ _ _ _ (ChatState user msgs)) ->
    s {_uiState = ChatState user (msgs <> [msg])}
  s -> s

chatInput :: AppState -> Widget Text
chatInput st =
  withBorderStyle BS.unicodeRounded $
    str ">>>"
      <+> renderEditor (vBox . map txt) True (st ^. msgEditor)

chatViewport :: ViewportScroll Text
chatViewport = viewportScroll "ChatViewport"

box :: Widget n -> Widget n
box = center . withBorderStyle BS.unicodeRounded . borderWithLabel (str "chat")

ui :: AppState -> Widget Text
ui s =
  case s ^. uiState of
    UsernameState -> box $ hLimit 20 $ userPrompt s
    ConnectingState -> box $ str "Connecting..."
    ChatState user msgs ->
      box $
        messages' msgs <=> hBorder <=> vLimit 1 (chatInput s)
  where
    messages' :: [UserMsg] -> Widget Text
    messages' msgs =
      viewport "ChatViewport" Vertical $ vBox $ map msgLine msgs

    msgLine :: UserMsg -> Widget Text
    msgLine msg = txt (msg ^. sender <> ": " <> msg ^. content)

userPrompt :: AppState -> Widget Text
userPrompt s =
  str "Enter your username:"
    <=> renderEditor (txt . mconcat) True (s ^. userNameEditor)

main :: IO ()
main = do
  eventChan' <- newBChan 10
  userNameMVar' <- newEmptyMVar
  sendChannel <- newBChan 10

  let initialState =
        AppState
          eventChan'
          sendChannel
          userNameMVar'
          (E.editor "msgEditor" Nothing "")
          (E.editor "userNameEditor" (Just 1) "")
          UsernameState
      buildVty = mkVty defaultConfig
  initialVty <- buildVty

  race_
    (void $ customMain initialVty buildVty (Just eventChan') app initialState)
    (webSocketThread userNameMVar' eventChan' sendChannel)
  where
    app :: App AppState Event Text
    app =
      App
        { appDraw = \s -> [center (ui s)]
        , appChooseCursor = \_ _ -> Nothing
        , appHandleEvent = handleEvent
        , appStartEvent = pure ()
        , appAttrMap = const attrMap'
        }

    attrMap' :: A.AttrMap
    attrMap' = A.attrMap V.defAttr []

webSocketThread :: MVar Text -> BChan Event -> BChan UserMsg -> IO ()
webSocketThread userNameMVar' bChan sendChannel = do
  userName <- readMVar userNameMVar'
  runClient "127.0.0.1" 8080 ("/" <> unpack userName) clientApp
  where
    clientApp :: ClientApp ()
    clientApp conn = do
      concurrently_
        (receiverThread conn)
        (senderThread conn)

    receiverThread :: Connection -> IO ()
    receiverThread conn = do
      msg' <- receiveData conn
      case decode msg' of
        Just Connected -> writeBChan bChan ConnectionReadyEvent
        Just (UserMessage msg) -> writeBChan bChan (MessageEvent msg)
        Nothing -> error $ "Error in decoding message " <> show msg'
      receiverThread conn

    senderThread :: Connection -> IO ()
    senderThread conn = do
      msg <- readBChan sendChannel
      sendBinaryData conn (encode msg)
      senderThread conn
