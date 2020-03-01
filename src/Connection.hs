module Connection
( Connection(..)
, ConnectionId
, Message
) where

import qualified EncryptionMock as E


type ConnectionId = String
type MessageId = String
type TimeStamp = String
type MessageData = String

data Connection = Connection
  { id :: ConnectionId
  , key :: Maybe E.PublicKey
  }

data Message = Message
  { connectionId :: ConnectionId
  , messageId :: MessageId
  , ts :: TimeStamp
  , msg :: MessageData
  }