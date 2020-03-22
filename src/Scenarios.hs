{-# LANGUAGE BlockArguments #-}

module Scenarios where

import Model
import Types


-- See "How Alice and Bob use simplex messaging protocol"
-- https://github.com/simplex-chat/protocol/blob/master/simplex-messaging.md#how-alice-and-bob-use-simplex-messaging-protocol
-- (steps are labelled below)
establishConnection :: (Client m, Model m) => m ()
establishConnection = do
  let srv = "https://alice.example.com/connection"
  server srv
  client "Alice"
  client "Bob"
  act         "Alice" $ addServer srv
  cid  <- act "Alice" $ requestConnection srv         -- 1
  oob  <- act "Alice" $ prepareOutOfBand srv cid      -- 2
  conn' <- act  "Bob" $ receiveOutOfBand oob          -- 3
  let (srv', cid') = conn'
  act           "Bob" $ confirmConnection srv' cid'
  sKey <- act "Alice" $ getSenderKey srv cid          -- 4
  act         "Alice" $ secureConnection srv cid sKey -- 5
  -- 6. The connection is now established
  act           "Bob" $ sendMessage srv' cid' "hello"
  msgs <- act "Alice" $ getMessages srv cid
  only1 msgs \msg ->
    if msg == "hello"
      then return ()
      else error $ "message received: " ++ msg


only1 :: [TextMessage] -> (TextMessage -> m ()) -> m ()
only1 msgs f =
  case msgs of
    [msg] -> f msg
    [] -> error "no messages"
    _ -> error "many messages"
