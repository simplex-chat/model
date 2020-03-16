{-# LANGUAGE TemplateHaskell #-}

module Connection
( Connection(..)
, ServerConnection
, ConnectionId
, Message
) where

import Control.Lens

import qualified EncryptionMock as E


type ConnectionId = String
type MessageId = String
type TimeStamp = String
type MessageData = String

data Connection = Connection
  { _cid :: ConnectionId
  , _key :: Maybe E.KeyPair
  } deriving (Show)
makeLenses ''Connection

data ServerConnection = ServerConnection
  { _recipient :: Connection
  , _sender :: Connection
  } deriving (Show)
makeLenses ''ServerConnection


data Message = Message
  { connectionId :: ConnectionId
  , messageId :: MessageId
  , ts :: TimeStamp
  , msg :: MessageData
  }
