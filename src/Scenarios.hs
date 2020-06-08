{-# LANGUAGE AllowAmbiguousTypes #-}

module Scenarios where

import Model
import Simplex.Messaging.Shared

import ClassyPrelude


-- See "How Alice and Bob use simplex messaging protocol"
-- https://github.com/simplex-chat/protocol/blob/master/simplex-messaging.md#how-alice-and-bob-use-simplex-messaging-protocol
-- (steps are labelled below)
establishConnection :: forall c m. Model m c => m ()
establishConnection = do
  let srv = "https://alice.example.com/connection"
  server @m @c srv
  client @m @c "Alice"
  client @m @c "Bob"
  act         "Alice" $ addServer @c srv
  cid  <- act "Alice" $ requestConnection @c srv         -- 1
  oob  <- act "Alice" $ prepareOutOfBand @c srv cid      -- 2
  conn' <- act  "Bob" $ receiveOutOfBand @c oob          -- 3
  let (srv', cid') = conn'
  act           "Bob" $ confirmConnection @c srv' cid'
  sKey <- act "Alice" $ getSenderKey @c srv cid          -- 4
  act         "Alice" $ secureConnection @c srv cid sKey -- 5
  -- 6. The connection is now established
  act           "Bob" $ sendMessage @c srv' cid' "hello"
  msgs <- act "Alice" $ getMessages @c srv cid
  only1 msgs \msg ->
    if msg == "hello"
      then return ()
      else error $ "message received: " ++ unpack msg


only1 :: [TextMessage] -> (TextMessage -> m ()) -> m ()
only1 msgs f =
  case msgs of
    [msg] -> f msg
    [] -> error "no messages"
    _ -> error "many messages"
