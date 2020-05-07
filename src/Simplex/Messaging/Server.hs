{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Simplex.Messaging.Server
  ( ServerStore(..)
  , ServerStoreException(..)
  , ServerConnection(..)
  , Store'
  ) where

import Simplex.Messaging.Shared
import Simplex.Messaging.Types (Message)

import ClassyPrelude
import UnliftIO.Exception()


data ServerConnection = ServerConnection
  { connectionId :: ConnectionId
  , senderConnectionId :: SenderConnectionId
  , recipientKey :: RecipientKey
  , senderKey :: Maybe SenderKey
  }

data ServerStoreException = ConnectionExists | NoConnection
                            | forall e. Exception e => ServerStoreException e
deriving instance Show ServerStoreException
deriving instance Exception ServerStoreException

type Store' m a = Monad m => m (Either ServerStoreException a)

class Monad m => ServerStore m where
  addConn       :: ServerConnection
                -> Store' m ()
  getConn       :: ConnectionId
                -> Store' m ServerConnection
  -- getSenderConn :: SenderConnectionId
  --               -> Store' m ServerConnection
  -- updateConn    :: ConnectionId
  --               -> (ServerConnection -> ServerConnection)
  --               -> Store' m ()
  -- deleteConn    :: ConnectionId
  --               -> Store' m ()
  -- addMsg        :: ConnectionId
  --               -> Message
  --               -> Store' m ()
  -- getMsgs       :: ConnectionId
  --               -> Maybe MessageId -- fromMessageId
  --               -> Store' m [Message]
  -- deleteMsg     :: ConnectionId
  --               -> MessageId
  --               -> Store' m ()
  -- createMsg     :: SenderConnectionId
  --               -> Message
  --               -> Store' m ()


data CryptoException = forall e. Exception e => CryptoException e
deriving instance Show CryptoException
deriving instance Exception CryptoException

class Monad m => Crypto m where
  randomConnIds :: m (Either CryptoException (ConnectionId, SenderConnectionId))

  
data ServerException = forall e. Exception e => ServerException e
deriving instance Show ServerException
deriving instance Exception ServerException

type Server' m a = m (Either ServerException a)

class (Crypto m, ServerStore m) => Server m where
  result :: a -> Server' m a
  result x = return $ Right x

  exception :: Exception e => e -> Server' m a
  exception e = return . Left $ ServerException e

  createConnection  :: RecipientKey
                    -> Server' m (ConnectionId, SenderConnectionId)
  createConnection key =
    randomConnIds >>= \case
      Left e -> exception e
      Right (connId, senderConnId) -> 
        addConn (ServerConnection connId senderConnId key Nothing) >>= \case
          Left ConnectionExists -> createConnection key
          Left e                -> exception e
          Right _               -> result (connId, senderConnId)

--   secureConnection  :: ConnectionId
--                     -> SenderKey
--                     -> Server' m ()
--   secureConnection connId key = return $ Right ()

--   deleteConnection  :: ConnectionId
--                     -> Server' m ()
--   deleteConnection connId = return $ Right ()

--   getMessages       :: ConnectionId
--                     -> Maybe MessageId
--                     -> Server' m [Message]
--   getMessages connId fromMessageId = return $ Right []

--   deleteMessage     :: ConnectionId
--                     -> MessageId
--                     -> Server' m ()
--   deleteMessage connId msgId = return $ Right ()

--   sendMessage       :: SenderConnectionId
--                     -> Message
--                     -> Server' m ()
--   sendMessage senderConnId msg = return $ Right ()
