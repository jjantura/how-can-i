{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Crypto.Cipher.ChaCha as CH
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.List            as L
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T

import           Numeric              (showHex)

key :: ByteString
key = BS.pack $ replicate 32 0

nonce :: ByteString
nonce = BS.pack $ replicate 8 0

initCipher :: CH.State
initCipher  = CH.initialize 20 key nonce

plainText :: ByteString
plainText = T.encodeUtf8 "test"

dumpHex :: ByteString -> IO ()
dumpHex bs = putStrLn $ concatMap ((\f -> f "")  . showHex) $ BS.unpack bs

main :: IO ()
main = do
  let st = initCipher
      (ba, st') = CH.generate st 30 :: (ByteString, CH.State)
      (ba', st'') = CH.combine st' plainText -- encrypt, just xor
      (ba'', st''') = CH.combine st' ba' -- decrypt, same as above

  dumpHex ba -- show keystream, for given key and nonce should be 76b8e0ada0f13d90405d6ae55386bd28bdd219b8a08ded1aa836efcc 8b770dc7da41597c5157488d7724e03fb8d84a376a43b8f41518a11c c387b669b2ee6586
  dumpHex ba' -- encrypted (xored)
  putStrLn $ T.unpack $ T.decodeUtf8 ba'' -- decrypted (xored again), should be same as plaintext
