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
type Key = String

data KeyPair = KeyPair
  { label :: KeyLabel
  , public :: Key
  , private :: Maybe Key
  } deriving (Show)


data Encrypted a = Encrypted KeyLabel a deriving (Show)

data Signed a = Signed KeyLabel a deriving (Show)


-- | Encrypt value with key
--
-- >>> encrypt (KeyPair "key" "123" (Just "456")) "test"
-- Encrypted "key" "test"
--
encrypt :: KeyPair -> a -> Encrypted a
encrypt key value = Encrypted (label key) value

-- | Decrypt value with key
--
-- >>> decrypt (KeyPair "key" "123" (Just "456")) (Encrypted "key" "test")
-- Right "test"
--
-- >>> decrypt (KeyPair "key" "123" (Just "456")) (Encrypted "another" "test")
-- Left "wrong key"
--
-- >>> decrypt (KeyPair "key" "123" Nothing) (Encrypted "key" "test")
-- Left "no private key"
--
decrypt :: KeyPair -> Encrypted a -> Either String a
decrypt key (Encrypted keyLabel value)
  | private key == Nothing = Left "no private key"
  | label key /= keyLabel  = Left "wrong key"
  | otherwise              = Right value


-- | Sign value using key
--
-- >>> sign (KeyPair "key" "123" (Just "456")) "test"
-- Right (Signed "key" "test")
--
-- >>> sign (KeyPair "key" "123" Nothing) "test"
-- Left "no private key"
--
sign :: KeyPair -> a -> Either String (Signed a)
sign key value
  | private key == Nothing = Left "no private key"
  | otherwise              = Right (Signed (label key) value)

-- | Verify signed value using key
--
-- >>> verify (KeyPair "key" "123" Nothing) (Signed "key" "test")
-- Right "test"
--
-- >>> verify (KeyPair "key" "123" Nothing) (Signed "another" "test")
-- Left "failed verification"
--
verify :: KeyPair -> Signed a -> Either String a
verify key (Signed keyLabel value)
  | label key /= keyLabel = Left "failed verification"
  | otherwise             = Right value

-- generateRandom :: RndState -> (Double, RndState)

-- createSeed :: IO RndState
