module Server
( Client(..)
, ClientRecipientConnection(..)
, ClientSenderConnection(..)
) where

data Client = Client
  { name :: String 
  }

data ClientRecipientConnection = ClientRecipientConnection
  { recipientURI :: String
  }

data ClientSenderConnection = ClientSenderConnection
  { senderURI :: String
  }
