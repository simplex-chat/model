{-# LANGUAGE AllowAmbiguousTypes #-}

module Model where

import Simplex.Messaging.Shared

import ClassyPrelude


class (Monad m, Client c) => Model m c where
  -- register server with the model
  server :: ServerUri -> m ()

  -- register client with the model
  client :: ClientName -> m ()

  -- perform client action
  act    :: ClientName -> c a -> m a


class Monad c => Client c where
  -- recipient: register server with the client
  addServer         :: ServerUri -> c ()

  -- recipient: request connection and stores it in the client storage as pending
  requestConnection :: ServerUri -> c ConnectionId

  -- recipient: secure connection for the sender to use
  secureConnection  :: ServerUri -> ConnectionId -> SenderKey -> c ()

  -- recipient: prepare out-of-band message
  -- (no calls to the server, ServerUri and ConnectionId identify connection in the client)
  prepareOutOfBand  :: ServerUri -> ConnectionId -> c OutOfBandMessage

  -- sender: receive out-of-band message from the recipient
  receiveOutOfBand  :: OutOfBandMessage -> c (ServerUri, SenderConnectionId)

  -- sender: confirm connection to recipient's server
  confirmConnection :: ServerUri -> SenderConnectionId -> c ()

  -- recipient: receive initial message with the sender key
  getSenderKey      :: ServerUri -> ConnectionId -> c SenderKey

  -- recipient: receive and deletes all messages from the connection
  getMessages       :: ServerUri -> ConnectionId -> c [TextMessage]

  -- sender: send the message to the server
  sendMessage       :: ServerUri -> SenderConnectionId -> TextMessage -> c ()


-- class Monad t => ServerTransport t where
--   createConnection :: ServerUri -> NewConnectionReqBody -> t NewConnectionResBody
--   secureConnection :: ServerUri -> ConnectionId -> SecureConnectionReqBody -> t ()