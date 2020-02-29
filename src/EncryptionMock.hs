module EncryptionMock where

type Key =
  { label :: String
  , key :: ByteString
  }

data KeyPair = KeyPair
  { public :: Key
  , private :: Key }

data Encrypted = Encrypted value keyLabel :: String

data Signed = Signed value keyLabel :: String

encrypt :: a -> Key -> Encrypted
encrypt value publicKey = Encrypted value publicKey.label

decrypt :: Encrypted -> Key -> a
decrypt (value keyLabel) privateKey{label, key} =
  if label == keyLabel then value else error "wrong key"
