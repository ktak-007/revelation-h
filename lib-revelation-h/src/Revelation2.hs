{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Revelation2 (encrypt, decrypt) where

import qualified Error

-- base
import           Control.Monad (replicateM)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Word (Word8)

-- mtl
import           Control.Monad.Except

-- zlib
import qualified Codec.Compression.Zlib as Zlib

-- crypton
import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (makeIV, Cipher(cipherInit), BlockCipher(cbcDecrypt, cbcEncrypt), IV)
import           Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import           Crypto.Hash (digestFromByteString, hashWith, SHA256(SHA256), Digest)
import           Crypto.KDF.PBKDF2 (fastPBKDF2_SHA1, Parameters(Parameters))
import           Crypto.Random (MonadRandom(..))

-- binary
import           Data.Binary.Get ( getRemainingLazyByteString
                                 , getWord8
                                 , runGetOrFail
                                 , skip
                                 , getByteString
                                 , lookAhead
                                 , Get
                                 )
-- memory
import qualified Data.ByteArray as BA

-- bytestring
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

header :: B.ByteString
header = "rvl"    -- magic string
      <> "\0"     -- separator
      <> "\2"     -- data version
      <> "\0"     -- separator
      <> "\0\4\7" -- application version
      <> "\0\0\0" -- separator

data ParsedInput = ParsedInput { appVersion :: [Word8]
                               , salt :: B.ByteString
                               , iv :: B.ByteString
                               , encodedData :: B.ByteString
                               } deriving Show

checkRvlMagicString :: Get ()
checkRvlMagicString = do
  magic <- lookAhead $ getByteString 3
  if magic /= "rvl"
  then fail Error.magicString
  else skip 3

checkZeroSeparator :: Int -> Get ()
checkZeroSeparator len = do
  bs <- lookAhead $ replicateM len getWord8
  if bs /= replicate len (0::Word8)
  then fail Error.format
  else skip len

checkDataVersionEq :: Word8 -> Get ()
checkDataVersionEq x = do
  b <- lookAhead getWord8
  if b /= x
  then fail Error.version
  else skip 1

getAppVersion :: Get [Word8]
getAppVersion = replicateM 3 getWord8

getSalt :: Get B.ByteString
getSalt = getByteString 8

getIV :: Get B.ByteString
getIV = getByteString 16

getRemainingEncodedData :: Get B.ByteString
getRemainingEncodedData = B.toStrict <$> getRemainingLazyByteString

parseInput :: Get ParsedInput
parseInput = ParsedInput <$  checkRvlMagicString
                         <*  checkZeroSeparator 1 
                         <*  checkDataVersionEq 2
                         <*  checkZeroSeparator 1
                         <*> getAppVersion
                         <*  checkZeroSeparator 3
                         <*> getSalt
                         <*> getIV
                         <*> getRemainingEncodedData

getParsedInput :: BL.ByteString -> ExceptT Error.Msg IO ParsedInput
getParsedInput input =
  case runGetOrFail parseInput input of
    Left (_, pos, msg) -> throwError $ msg <> " at position " <> show pos
    Right (_, _, parsed) -> return parsed

decrypt :: BL.ByteString -> B.ByteString -> ExceptT Error.Msg IO BL.ByteString
decrypt rawEncodedInput password = do
  input <- getParsedInput rawEncodedInput
  encodedData <- if BA.length input.encodedData `mod` 16 /= 0
                 then throwError Error.format
                 else return input.encodedData
  initialVector <- let iv = makeIV input.iv :: Maybe (IV AES256)
                   in case iv of
                      Nothing  -> throwError Error.initialVector
                      Just iv' -> return iv'
  cipher <- let pbkdf2 = getPBKF2 password input.salt
            in case cipherInit pbkdf2 of
               CryptoFailed e -> throwError $ Error.cipherInit <> ": " <> show e
               CryptoPassed c -> return c
  let (h, decodedData) = BA.splitAt 32 $ cbcDecrypt cipher initialVector encodedData
  case digestFromByteString h of
      Nothing -> throwError Error.hash256
      Just hash256 -> if hash256 /= hashWith SHA256 decodedData
                      then throwError Error.password
                      else decompress decodedData

getPBKF2 :: B.ByteString -> B.ByteString -> B.ByteString
getPBKF2 = fastPBKDF2_SHA1 (Parameters 12000 32)

decompress :: B.ByteString -> ExceptT Error.Msg IO BL.ByteString
decompress compressedData = do
  let padlen = B.last compressedData
  if B.any (/=padlen) (B.takeEnd (fromIntegral padlen) compressedData)
  then throwError Error.format
  else return $ Zlib.decompress $ BL.fromStrict $ B.dropEnd (fromIntegral padlen) compressedData

encrypt :: (MonadRandom m, MonadIO m) => BL.ByteString -> B.ByteString -> ExceptT Error.Msg m BL.ByteString
encrypt input password = do
  salt <- liftIO $ getRandomBytes 8
  iv <- liftIO $ getRandomBytes 16
  encrypt' input salt iv password

encrypt' :: MonadRandom m
         => BL.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
         -> ExceptT Error.Msg m BL.ByteString
encrypt' input salt iv password = do
  cipher <- case cipherInit $ getPBKF2 password salt of
                 CryptoFailed e -> throwError $ Error.cipherInit <> ": " <> show e
                 CryptoPassed c -> return c
  initialVector <- case makeIV iv :: Maybe (IV AES256) of
                        Nothing  -> throwError Error.initialVector
                        Just iv' -> return iv'
  let compressedInput = BL.toStrict $ Zlib.compress input
      padlen = 16 - (B.length compressedInput `mod` 16)
      padding = B.replicate padlen $ fromIntegral padlen
      dataToEncode = compressedInput <> padding
      digest = digestToByteString $ hashWith SHA256 dataToEncode
  return $ BL.fromStrict
         $ header <> salt <> iv <>
           cbcEncrypt cipher initialVector (digest <> dataToEncode)

digestToByteString :: Digest SHA256 -> B.ByteString
digestToByteString = B.pack . BA.unpack
