module Client
( Client(..)
, ClientName
, makeClient
, addServer
-- , ClientRecipientConnection(..)
-- , ClientSenderConnection(..)
) where

import Control.Lens
import Control.Monad.State
import Data.Function
import Data.Map as Map

import Connection
import qualified Model as M
import qualified Server as S

type ClientName = String

data Client = Client
  { _name :: ClientName
  , _servers :: Map.Map S.ServerUri ()
  , _pendingRecipientConnections :: Map.Map ConnectionId ServerConnection
  , _recipientConnections :: Map.Map ConnectionId ServerConnection
  } deriving (Show, M.Model)
makeLenses ''Client

makeClient :: ClientName -> Client
makeClient name' = Client name' Map.empty Map.empty Map.empty

addServer :: S.ServerUri -> State Client ()
addServer uri = M.addItem servers uri ()

-- requestConnection :: S.ServerUri -> String -> State Client ConnectionId
-- requestConnection uri lbl = do
--   let recipientKey = E.newKeyPair lbl
--   connResponse <- M.itemAction servers uri (S.createConnection $ public recipientKey)
--   let connId = view (recipient . cid) conn
--   M.addItem pendingRecipientConnections lbl conn
--   return connId

data ClientRecipientConnection = ClientRecipientConnection
  { recipientURI :: String
  , senderURI :: Maybe String
  -- , recipientKey :: E.KeyPair
  }

-- data ClientSenderConnection = ClientSenderConnection
--   { senderURI :: String
--   }
