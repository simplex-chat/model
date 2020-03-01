{-# LANGUAGE DataKinds #-}

module EncryptionMock
( Key
, KeyPair
, Encrypted
, Signed
, encrypt
, decrypt
, sign
, verify
) where

data Key = Key
  { label :: String
  , key :: String
  }

data KeyPair = KeyPair
  { public :: Key
  , private :: Key
  }

data Encrypted a = Encrypted String a

data Signed a = Signed String a

encrypt :: Key -> a -> Encrypted a
encrypt publicKey value = Encrypted (label publicKey) value

decrypt :: Key -> Encrypted a -> a
decrypt privateKey (Encrypted keyLabel value) =
  if (label privateKey) == keyLabel then value else error "wrong key"

sign :: Key -> a -> Signed a
sign privateKey value = Signed (label privateKey) value

verify :: Key -> Signed a -> Bool
verify publicKey (Signed keyLabel _) = (label publicKey) == keyLabel
