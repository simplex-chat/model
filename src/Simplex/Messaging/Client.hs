{-# LANGUAGE ExistentialQuantification #-}

module Simplex.Messaging.Client where

import Simplex.Messaging.Shared
import Simplex.Messaging.Types

import ClassyPrelude


data RecipientConnectionStatus =
  NewConnection  -- connection created
  | SentToSender -- out-of-band message sent to sender
  | Confirmed    -- sender confirmation received
  | Secured      -- secured on the server
  | Active       -- sender is sending messages after connection is secured
  | Disabled
  | Deleted

data RecipientConnection = RecipientConnection
  { serverUri    :: ServerUri
  , connectionId :: ConnectionId
  , key          :: RecipientKey
  , status       :: [(RecipientConnectionStatus, TimeStamp)]
  }


data SenderConnectionStatus =
  ReceivedFromRecepient  -- out-of-band message received from recipient
  | ConfirmationFailed   -- sending confirmation failed
  | ConfirmedToRecipient -- sender confirmation sent
  | SecuredByRecipient   -- recipient secured the connection
  | Unavailable          -- recipient disabled or deleted the connection

data SenderConnection = SenderConnection
  { serverUri    :: ServerUri
  , connectionId :: SenderConnectionId
  , key          :: SenderKey
  , status       :: [(SenderConnectionStatus, TimeStamp)]
  }


data ClientStoreException = forall e. Exception e => ServerStoreException e
deriving instance Show ClientStoreException
deriving instance Exception ClientStoreException

type ClientStore' m a = Monad m => m (Either ClientStoreException a)

class Monad m => ClientStore m where
  addRecConn  :: RecipientConnection
              -> ClientStore' m ()
  addSendConn :: SenderConnection
              -> ClientStore' m ()
  getRecConn  :: ServerUri
              -> ConnectionId
              -> ClientStore' m RecipientConnection
  getSendConn :: ServerUri
              -> SenderConnectionId
              -> ClientStore' m SenderConnection


data ClientException = forall e. Exception e => ClientException e
deriving instance Show ClientException
deriving instance Exception ClientException

type Client' m a = m (Either ClientException a)


class Monad m => Client m where
  -- recipient: register server with the client
  addServer         :: ServerUri
                    -> Client' m ()

  -- recipient: request connection and stores it in the client storage as pending
  requestConnection :: ServerUri 
                    -> Client' m ConnectionId

  -- recipient: secure connection for the sender to use
  secureConnection  :: ServerUri
                    -> ConnectionId
                    -> SenderKey
                    -> Client' m ()

  -- recipient: prepare out-of-band message
  -- (no calls to the server, ServerUri and ConnectionId identify connection in the client)
  prepareOutOfBand  :: ServerUri
                    -> ConnectionId
                    -> Client' m OutOfBandMessage

  -- sender: receive out-of-band message from the recipient
  receiveOutOfBand  :: OutOfBandMessage
                    -> Client' m (ServerUri, SenderConnectionId)

  -- sender: confirm connection to recipient's server
  confirmConnection :: ServerUri
                    -> SenderConnectionId
                    -> Client' m ()

  -- recipient: receive initial message with the sender key
  getSenderKey      :: ServerUri
                    -> ConnectionId
                    -> Client' m SenderKey

  -- recipient: receive and deletes all messages from the connection
  getMessages       :: ServerUri
                    -> ConnectionId
                    -> Client' m [TextMessage]

  -- sender: send the message to the server
  sendMessage       :: ServerUri
                    -> SenderConnectionId
                    -> TextMessage
                    -> Client' m ()
  