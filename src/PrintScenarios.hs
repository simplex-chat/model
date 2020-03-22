{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PrintScenarios where

import Model
import Types
import Control.Monad.Writer
import Scenarios

type Printer = Writer [String]

instance Model Printer where
  server :: ServerUri -> Printer ()
  server srv = tell ["Server: " ++ srv]

  client :: ClientName -> Printer ()
  client name = tell ["Client: " ++ name]

  act    :: ClientName -> Printer a -> Printer a
  act name action = do
    tell ["\n" ++ name ++ ":"]
    action


instance Client Printer where
  addServer         :: ServerUri -> Printer ()
  addServer srv = tell ["  - add server " ++ srv ++ " to the client"]

  requestConnection :: ServerUri -> Printer ConnectionId
  requestConnection srv = do
    tell ["  - request connection from server " ++ srv]
    let cid = "Qxz93A" -- connection recipient ID
    tell ["    returning connection ID, e.g. " ++ cid]
    return cid

  secureConnection  :: ServerUri -> ConnectionId -> SenderKey -> Printer ()
  secureConnection srv cid sKey =
    tell ["  - secure with sender key " ++ sKey ++ " connection " ++ srv ++ "/" ++ cid]

  prepareOutOfBand  :: ServerUri -> ConnectionId -> Printer OutOfBandMessage
  prepareOutOfBand srv cid = do
    tell ["  - prepare out-of-band message for the sender"]
    tell ["    to confirm connection " ++ srv ++ "/" ++ cid]
    let cid' = "N9pA3g" -- sender ID is different from recipient ID
    tell ["    sender ID is different, e.g. " ++ cid']
    return (OutOfBandMessage srv cid')

  receiveOutOfBand  :: OutOfBandMessage -> Printer (ServerUri, SenderConnectionId)
  receiveOutOfBand oobMsg = do
    tell ["  - receive out-of-band message from the recipient"]
    let (OutOfBandMessage srv' cid') = oobMsg
    tell ["    with the connection " ++ srv' ++ "/" ++ cid']
    return (srv', cid')

  confirmConnection :: ServerUri -> SenderConnectionId -> Printer ()
  confirmConnection srv cid =
    tell ["  - confirm connection " ++ srv ++ "/" ++ cid]

  getSenderKey      :: ServerUri -> ConnectionId -> Printer SenderKey
  getSenderKey srv cid = do
    let sKey = "XPaVEVNunkYKqqK0dnAT5Q"
    tell ["  - receive sender key " ++ sKey ++ " for connection " ++ srv ++ "/" ++ cid]
    return sKey

  getMessages       :: ServerUri -> ConnectionId -> Printer [TextMessage]
  getMessages srv cid = do
    tell ["  - receive messages from connection " ++ srv ++ "/" ++ cid]
    let msg = "hello"
    tell ["    message " ++ (show msg) ++ " received"]
    return [msg]

  sendMessage       :: ServerUri -> SenderConnectionId -> TextMessage -> Printer ()
  sendMessage srv' cid' msg = do
    tell ["  - send message " ++ (show msg) ++ " to connection " ++ srv' ++ "/" ++ cid']
