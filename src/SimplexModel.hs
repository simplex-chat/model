module SimplexModel
( SimplexModel(..)
, scenario
) where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map

import qualified Server as S
import qualified Client as C
import qualified Model as M

type ServersMap = Map.Map S.ServerUri S.Server
type ClientsMap = Map.Map String C.Client

-- data ServerState = ServerState ConnectionMap MessageMap

data SimplexModel = SimplexModel
  { _servers :: ServersMap
  , _clients :: ClientsMap
  } deriving (Show, M.Model, M.MutableModel)
makeLenses ''SimplexModel


scenario :: State SimplexModel ()
scenario = do
  defineServer "https://alice.example.com/connection"
  defineClient "Alice"
  defineClient "Bob"
  clientAction "Alice" (C.addServer "https://alice.example.com/connection")
  -- connId <- clientAction "Alice" (C.requestConnection "https://alice.example.com/connection")
  -- subscr <- clientAction "Alice" (C.subscribeConnection conn)
  -- oobConn <- clientAction "Alice" (C.prepareConnectionOutOfBand connId)
  -- clientAction "Bob" (C.receiveConnectionOutOfBand oobConn)


defineServer :: S.ServerUri -> State SimplexModel ()
defineServer uri = M.addItem servers uri (S.Server uri)

defineClient :: C.ClientName -> State SimplexModel ()
defineClient name = M.addItem clients name (C.makeClient name)

clientAction :: C.ClientName -> State C.Client () -> State SimplexModel ()
clientAction name action = M.itemAction clients name action

serverAction :: S.ServerUri -> State S.Server () -> State SimplexModel ()
serverAction uri action = M.itemAction servers uri action
