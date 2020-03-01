module Main where

import qualified EncryptionMock as E
import qualified Server as S


main :: IO ()
main = do
  putStrLn "hello world"

data Client = Client
  { name :: String 
  }

data ClientRecipientConnection = ClientRecipientConnection
  { recipientURI :: String
  }

data ClientSenderConnection = ClientSenderConnection
  { senderURI :: String
  }
