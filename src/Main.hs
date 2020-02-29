module Main where

main :: IO ()
main = do
  putStrLn "hello world"

data Client = Client
  { name :: String 
  }

data Server = Server
  { uri :: String -- create connection URI
  }

data ServerConnection = ServerConnection
  { recipientId :: String
  , senderId :: String 
  }

data ClientRecipientConnection = ClientRecipientConnection
  { recipientURI :: String
  }

data ClientSenderConnection = ClientSenderConnection
  { senderURI :: String
  }

data DuplexConnection = DuplexConnection
  { receive :: ClientRecipientConnection
  , send :: ClientSenderConnection
  }
