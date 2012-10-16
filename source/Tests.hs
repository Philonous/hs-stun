{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
module Tests where

import qualified Network.Stun.Serialize as Stun
import Test.QuickCheck
import Data.Serialize

import qualified Data.ByteString as BS
import Control.Monad

instance Arbitrary Stun.MessageClass where
    arbitrary = elements [ Stun.Request
                         , Stun.Success
                         , Stun.Failure
                         , Stun.Indication]




checkEncDec method messageClass = let method' = method `mod` (2^12) in
    (method', messageClass) ==
      (Stun.decodeMessageType $ Stun.encodeMessageType method' messageClass)

checkDecEnc word = let word' = word `mod` (2^14) in
        word' == (uncurry Stun.encodeMessageType $ Stun.decodeMessageType word')


check1 = quickCheckWith (stdArgs{maxSuccess = 1000 }) checkEncDec
check2 = quickCheckWith (stdArgs{maxSuccess = 1000 }) checkDecEnc

instance Arbitrary Stun.Message where
    arbitrary = do
        messageMethod <- (`mod` (2^12)) `liftM` arbitrary
        messageClass <- arbitrary
        transactionId <- arbitrary
        messageAttributes <- arbitrary
        return Stun.Message{..}


checkSerializer header = decode (encode header) == Right header

check3 = quickCheckWith (stdArgs{maxSuccess = 1000 })
              (checkSerializer :: Stun.Message -> Bool)

instance Arbitrary Stun.Attribute where
    arbitrary = liftM2 Stun.Attribute arbitrary (BS.pack `liftM` arbitrary)

check4 = quickCheckWith (stdArgs{maxSuccess = 1000 })
              (checkSerializer :: Stun.Attribute -> Bool)
