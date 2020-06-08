{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Simplex.Messaging.Model.Server where

import Simplex.Messaging.Server
import Simplex.Messaging.Shared
import Simplex.Messaging.Types (Message)

import ClassyPrelude
import Control.Concurrent.STM
import Control.Lens
import Data.Generics.Product.Fields
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M

data StoreData = StoreData
                  { senderIds :: Map SenderConnectionId ConnectionId
                  , connections :: Map ConnectionId ServerConnection
                  , messages :: Map ConnectionId [Message]
                  }

type TStore = TVar StoreData

initialize :: STM TStore
initialize = newTVar $ StoreData M.empty M.empty M.empty

type HasStore env = HasField' "stmServerStore" env TStore
type STMStore' env a = HasStore env => ReaderT env STM (Either ServerStoreException a)

instance HasStore env => ServerStore (ReaderT env STM) where
  addConn :: ServerConnection
          -> STMStore' env ()
  addConn c@ServerConnection{senderConnectionId, connectionId} = stateStore \st ->
    let ss = st & senderIds
        cs = st & connections
        newIds = isNothing (M.lookup senderConnectionId ss) && isNothing (M.lookup connectionId cs)
    in
      if newIds then
        let st' = st { senderIds = M.insert senderConnectionId connectionId ss
                      , connections = M.insert connectionId c cs }
        in (Right (), st')
      else
        (Left ConnectionExists, st)

  getConn :: ConnectionId
          -> STMStore' env ServerConnection
  getConn connId = readStore \st ->
    maybe (Left NoConnection) Right $ M.lookup connId (st & connections)


getStore :: HasStore env => ReaderT env STM TStore
getStore = asks (^. field' @"stmServerStore")

stateStore :: HasStore env
           => (StoreData -> (Either ServerStoreException a, StoreData))
           -> STMStore' env a
stateStore f = getStore >>= lift . (`stateTVar` f)

readStore :: HasStore env
          => (StoreData -> Either ServerStoreException a)
          -> STMStore' env a
readStore f = fmap f $ getStore >>= lift . readTVar
