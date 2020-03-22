module Types where

import Simplex.Messaging.Types

type ServerUri = String
type ClientName = String
type ConnectionId = Base64EncodedString
type SenderConnectionId = Base64EncodedString
type MessageId = Base64EncodedString
type PublicKey = Base64EncodedString
type RecipientKey = PublicKey
type SenderKey = PublicKey
type TextMessage = String
type Error = String

data OutOfBandMessage = OutOfBandMessage ServerUri SenderConnectionId
