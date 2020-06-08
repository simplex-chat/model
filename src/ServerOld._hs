{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module ServerOld
( Server(..)
, ServerUri
-- , ServerState
-- , createConnection
) where

-- import Connection
  
-- import qualified EncryptionMock as E
-- import qualified Data.Map as Map
-- import Data.Function
-- import GHC.Records


type ServerUri = String

data Server = Server
  { uri :: ServerUri -- create connection URI
  } deriving (Show)


-- createConnection :: State Server ServerConnection
-- createConnection = return $ newServerConnection 

-- data ServerConnection = ServerConnection
--   { recipient :: Connection
--   , sender :: Connection
--   }

-- type ConnectionMap = Map.Map ConnectionId ServerConnection
-- type MessageMap = Map.Map ConnectionId [Message]

-- data ServerState = ServerState ConnectionMap MessageMap


-- newConnection :: Maybe E.PublicKey -> Connection
-- newConnection maybeKey = Connection{cid="", key=maybeKey}

-- newServerConnection :: E.PublicKey -> ServerConnection
-- newServerConnection recipientKey = ServerConnection
--                                     { recipient = newConnection (Just recipientKey)
--                                     , sender = newConnection Nothing
--                                     }

-- addServerConnection :: ServerConnection -> ServerState -> ServerState
-- addServerConnection conn (ServerState conns msgs) =
--   if Map.member recipientId conns
--     then error "connection exists"
--     else ServerState (Map.insert recipientId conn conns) msgs
--       where recipientId = conn & recipient & cid

-- secureServerConnection :: ConnectionId -> E.PublicKey -> ServerState -> ServerState
-- secureServerConnection connId senderKey (ServerState conns msgs) =
--   if Map.member connId conns
--     then ServerState (Map.adjust update connId conns) msgs
--     else error "connection does not exist"
--       where
--         update :: ServerConnection -> ServerConnection
--         update conn =
--           if (conn & sender & key) /= Nothing
--             then error "connection already secured"
--             else let sender' = sender conn
--                  in conn{sender = sender'{key = Just senderKey}}


-- data CreateConnReq = CreateConnReq {recipientKey :: E.PublicKey}

-- createConnection :: E.Signed CreateConnReq -> Server -> ServerState -> (ServerConnection, ServerState)
-- createConnection req server state =
--   if E.verify key req
--     then (conn, addServerConnection conn state)
--     else error "Bad Request"
--       where key = recipientKey (E.signedValue req)
--             conn = newServerConnection key

-- -- data Verified a = Verified a

-- verifyRecipientRequest ::  HasField "connectionId" a ConnectionId => E.Signed a -> ServerState -> Bool
-- verifyRecipientRequest req (ServerState conns _) = E.verify recipientKey req
--   where
--     connId = getField @"connectionId" $ E.signedValue req
--     Just conn = Map.lookup connId conns
--     Just recipientKey = key . recipient $ conn

-- data SecureConnReq = SecureConnReq
--   { connectionId :: ConnectionId
--   , senderKey :: E.PublicKey
--   }

-- secureConnection :: E.Signed SecureConnReq -> Server -> ServerState -> ServerState
-- secureConnection req server state@(ServerState conns msgs) =
--   if verifyRecipientRequest req state
--     then secureServerConnection connId (senderKey reqBody) state
--     else error "Bad Request"
--       where
--         reqBody = E.signedValue req
--         connId = connectionId reqBody


-- -- ExceptT
-- -- EitherT
