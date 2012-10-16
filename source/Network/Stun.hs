{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Network.Stun where

import Network
import Data.Word
import Data.Serialize
import Data.Bits

import Numeric


import qualified Data.ByteString as BS

data MessageClass = Request
                  | Success
                  | Failure
                  | Indication
                    deriving (Show, Eq)


cookie = 0x2112A442 :: Word32

type Method = Word16

encodeMessageType :: Method -> MessageClass -> Word16
encodeMessageType method messageClass =
    (method .&. 0xf)                    -- least 4 bits remain the same
    .|. (c0 `shiftL` 4)                 -- bit 5 is class low bit
    .|. ((method .&. 0x70)  `shiftL` 1) -- next 3 bits are offset by 1
    .|. (c1 `shiftL` 8)                 -- bit 9 is class high bit
    .|. ((method .&. 0xf80) `shiftL` 2) -- highest 5 bits are offset by 2
  where
    (c1, c0) = case messageClass of
        Request    -> (0,0)
        Success    -> (1,0)
        Failure    -> (1,1)
        Indication -> (0,1)

decodeMessageType :: Word16 -> (Method, MessageClass)
decodeMessageType word = (method, mClass)
  where
    mClass = case (c1, c0) of
        (False ,False) -> Request
        (True  ,False) -> Success
        (True  ,True) -> Failure
        (False ,True) -> Indication
    c0 = testBit word 4
    c1 = testBit word 8
    method =
        (word .&. 0xf)                     -- least 4 bits remain the same
        .|. ((word .&. 0xe0)  `shiftR` 1)  -- next 3 bits are offset by 1
        .|. ((word .&. 0x3e00) `shiftR` 2) -- highest 5 bits are offset by 2



putMessage method messageClass messageLength = do
    putWord16be (encodeMessageType method messageClass)
    putWord16be messageLength
    putWord32be cookie


showBits a = reverse [if testBit a i then '1' else '0' | i <- [0.. bitSize a - 1]]