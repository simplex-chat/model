{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Scenarios where

import Model
import Types


-- See "How Alice and Bob use simplex messaging protocol"
-- https://github.com/simplex-chat/protocol/blob/master/simplex-messaging.md#how-alice-and-bob-use-simplex-messaging-protocol
-- (steps are labelled below)
establishConnection :: forall c m. (Client c, Model m) => m ()
establishConnection = do
  let srv = "https://alice.example.com/connection"
  server srv
  client "Alice"
  client "Bob"
  act         "Alice" $ addServer @c srv
  cid  <- act "Alice" $ requestConnection @c srv    -- 1
  oob  <- act "Alice" $ prepareOutOfBand @c srv cid -- 2
  conn' <- act  "Bob" $ receiveOutOfBand @c oob     -- 3
  let (srv', cid') = conn'
  act           "Bob" $ confirmConnection @c srv' cid'
  msgs <- act "Alice" $ getMessages @c srv cid      -- 4
  only1 msgs \senderKey -> do
    act          "Alice" $ secureConnection @c srv cid senderKey -- 5
    -- 6. The connection is now established
    act            "Bob" $ sendMessage @c srv' cid' "hello"
    msgs' <- act "Alice" $ getMessages @c srv cid
    only1 msgs' \msg ->
      if msg == "hello"
        then return ()
        else error $ "message received: " ++ msg


only1 :: [TextMessage] -> (TextMessage -> m ()) -> m ()
only1 msgs f =
  case msgs of
    [msg] -> f msg
    [] -> error "no messages"
    _ -> error "many messages"
