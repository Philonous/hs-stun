module Tests where

import qualified Network.Stun as Stun
import Test.QuickCheck

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