{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
module Tests where

import qualified Network.Stun as Stun
import Test.QuickCheck
import Data.Serialize

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

instance Arbitrary Stun.MessageHeader where
    arbitrary = do
        messageMethod <- (`mod` (2^12)) `liftM` arbitrary
        messageClass <- arbitrary
        messageLength <- arbitrary
        messageId <- arbitrary
        return Stun.MessageHeader{..}


checkSerializer header = decode (encode header) == Right header

check3 = quickCheckWith (stdArgs{maxSuccess = 1000 })
              (checkSerializer :: Stun.MessageHeader -> Bool)
