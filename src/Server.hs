module Server
( Server
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

type MessageMap = Map.Map ConnectionId [Message]

data ServerState = ServerState [ServerConnection] MessageMap


newConnection :: Maybe E.PublicKey -> Connection
newConnection maybeKey = Connection{Connection.id="", key=maybeKey}

newServerConnection :: E.PublicKey -> ServerConnection
newServerConnection recipientKey = ServerConnection
                                    { recipient = newConnection (Just recipientKey)
                                    , sender = newConnection Nothing
                                    }

data CreateConnReq = CreateConnReq
  { recipientKey :: E.PublicKey
  }

createConnection :: E.Signed CreateConnReq -> Server -> ServerState -> (ServerConnection, ServerState)
createConnection req server (ServerState conns msgs) =
  if E.verify key req then (conn, ServerState (conn:conns) msgs) else error "Bad Request"
    where key = recipientKey (E.signedValue req)
          conn = newServerConnection key
