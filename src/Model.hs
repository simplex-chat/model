module Model where

import Types


class Monad m => Model m where
  -- register server with the model
  server :: ServerUri -> m ()

  -- register client with the model
  client :: ClientName -> m ()

  -- perform client action
  act    :: Client c => ClientName -> c a -> m a


class Monad m => Client m where
  -- recipient: register server with the client
  addServer         :: ServerUri -> m ()

  -- recipient: request connection and stores it in the client storage as pending
  requestConnection :: ServerUri -> m ConnectionId

  -- recipient: secure connection for the sender to use
  secureConnection  :: ServerUri -> ConnectionId -> SenderKey -> m ()

  -- recipient: prepare out-of-band message
  -- (no calls to the server, ServerUri and ConnectionId identify connection in the client)
  prepareOutOfBand  :: ServerUri -> ConnectionId -> m OutOfBandMessage

  -- sender: receive out-of-band message from the recipient
  receiveOutOfBand  :: OutOfBandMessage -> m (ServerUri, SenderConnectionId)

  -- sender: confirm connection to recipient's server
  confirmConnection :: ServerUri -> SenderConnectionId -> m ()

  -- recipient: receive and deletes all messages from the connection
  getMessages       :: ServerUri -> ConnectionId -> m [TextMessage]

  -- sender: send the message to the server
  sendMessage       :: ServerUri -> SenderConnectionId -> TextMessage -> m ()
