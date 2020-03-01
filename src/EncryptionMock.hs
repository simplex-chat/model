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

type KeyLabel = String

data Key = Key
  { label :: KeyLabel
  , key :: String
  } deriving (Show)

type PublicKey = Key

type PrivateKey = Key

data KeyPair = KeyPair
  { public :: PublicKey
  , private :: PrivateKey
  } deriving (Show)


data Encrypted a = Encrypted KeyLabel a deriving (Show)

data Signed a = Signed KeyLabel a deriving (Show)

encrypt :: PublicKey -> a -> Encrypted a
encrypt key value = Encrypted (label key) value

decrypt :: PrivateKey -> Encrypted a -> a
decrypt key (Encrypted keyLabel value) =
  if label key == keyLabel then value else error "wrong key"

sign :: PrivateKey -> a -> Signed a
sign key value = Signed (label key) value

verify :: PublicKey -> Signed a -> Bool
verify key (Signed keyLabel _) = label key == keyLabel
