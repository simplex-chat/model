module Server
( Server
, createConnection
) where

import qualified EncryptionMock as E
import Connection
import qualified Data.Map as Map


data Server = Server
  { uri :: String -- create connection URI
  }

data ServerConnection = ServerConnection
  { recipient :: Connection
  , sender :: Connection
  }

type ConnectionMap = Map.Map ConnectionId ServerConnection
type MessageMap = Map.Map ConnectionId [Message]

data ServerState = ServerState ConnectionMap MessageMap


newConnection :: Maybe E.PublicKey -> Connection
newConnection maybeKey = Connection{cid="", key=maybeKey}

newServerConnection :: E.PublicKey -> ServerConnection
newServerConnection recipientKey = ServerConnection
                                    { recipient = newConnection (Just recipientKey)
                                    , sender = newConnection Nothing
                                    }

_addServerConnection :: ServerConnection -> ServerState -> ServerState
_addServerConnection conn (ServerState conns msgs) =
  if Map.member recipientId conns
    then error "connection exists"
    else ServerState (Map.insert recipientId conn $ conns) msgs
      where recipientId = cid . recipient $ conn

-- _secureServerConnection :: ConnectionId -> E.PublicKey -> ServerState -> ServerState
-- _secureServerConnection connId senderKey (ServerState conns msgs) =
--   if Map.member connId conns
--     then ServerState (Map.adjust update connId conns) msgs
--     else error "connection does not exist"
--       where
--         update :: ServerConnection -> ServerConnection
--         update conn =
--           if (key . sender $ conn) /= Nothing
--             then error "connection already secured"
--             else ServerConnection
--                   { recipient = recipient conn
--                   , sender = Connection
--                               { cid = (cid . sender $ conn)
--                               , key = Just senderKey
--                               }
--                   }


data CreateConnReq = CreateConnReq {recipientKey :: E.PublicKey}

createConnection :: E.Signed CreateConnReq -> Server -> ServerState -> (ServerConnection, ServerState)
createConnection req server state =
  if E.verify key req
    then (conn, _addServerConnection conn state)
    else error "Bad Request"
      where key = recipientKey (E.signedValue req)
            conn = newServerConnection key


_verifyRecipientRequest :: E.Signed SecureConnReq -> ServerState -> Bool
_verifyRecipientRequest req (ServerState conns _) = E.verify recipientKey req where
  connId = connectionId (E.signedValue req)
  Just conn = Map.lookup connId conns
  Just recipientKey = key . recipient $ conn

data SecureConnReq = SecureConnReq
  { connectionId :: ConnectionId
  , senderKey :: E.PublicKey
  }

-- secureConnection :: E.Signed SecureConnReq -> Server -> ServerState -> ServerState
-- createConnection req server (ServerState conns msgs) =
--   if E.verify recipientKey req
--     then _secureServerConnection recipientId senderKey
--     else error "Bad Request"
--       where
--         reqBody = E.signedValue req
--         connectionId = connectionId reqBody
--         recipientKey = key . recipient $ Map.lookup connectionId conns