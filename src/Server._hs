module Server where

import Types
import Simplex.Messaging.Types

class Monad m => Server m where
  createConnection    :: RecipientKey -> m (ConnectionId, SenderConnectionId)
  secureConnection    :: ConnectionId -> SenderKey -> m ()
  deleteConnection    :: ConnectionId -> m ()
  getMessages         :: ConnectionId -> m [Message]
  deleteMessage       :: ConnectionId -> MessageId -> m ()
  sendMessage         :: SenderConnectionId -> Message -> m ()
