{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Lens.Micro.TH (makeLenses)

data UserMsg = UserMsg
  { _content :: Text
  , _sender :: Text
  }
  deriving (Show, Generic)

data Message
  = Connected
  | UserMessage UserMsg
  deriving (Generic, Show)

makeLenses ''UserMsg

instance ToJSON Message
instance FromJSON Message

instance ToJSON UserMsg where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON UserMsg where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}
