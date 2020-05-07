module Simplex.Messaging.Shared where

import Simplex.Messaging.Types

import ClassyPrelude

type ServerUri = Text
type ClientName = Text
type ConnectionId = Base64EncodedString
type SenderConnectionId = ConnectionId
type MessageId = Base64EncodedString
type PublicKey = Base64EncodedString
type RecipientKey = PublicKey
type SenderKey = PublicKey
type TextMessage = Text
type Error = Text

data OutOfBandMessage = OutOfBandMessage ServerUri SenderConnectionId
