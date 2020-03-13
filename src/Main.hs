module Main where

import qualified Data.Map as Map
import qualified EncryptionMock as E
-- import qualified Server as S
-- import qualified Client as C


main :: IO ()
main = do
  putStrLn "hello world"

data Server = undefined
data Client = undefined

type ServerUri = String
type ClientId = String
type ServersMap = Map.Map ServerUri Server
type ClientsMap = Map.Map ClientId Client

data ServerState = ServerState ConnectionMap MessageMap

data Model = Model
  { servers :: ServersMap
  , clients :: ClientsMap
  }

