{-# LANGUAGE TemplateHaskell #-}

module Connection
( Connection(..)
, ServerConnection
, ConnectionId
-- , Message
) where

-- import Control.Lens

import qualified EncryptionMock as E
import Types


data Connection = Connection
  { _cid :: ConnectionId
  , _key :: Maybe E.KeyPair
  } deriving (Show)
-- makeLenses ''Connection

data ServerConnection = ServerConnection
  { _recipient :: Connection
  , _sender :: Connection
  } deriving (Show)
-- makeLenses ''ServerConnection
