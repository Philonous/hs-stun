{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Stun where

import Network
import Control.Monad
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
    -- most significant 2 bits reamin 0
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
        (False, False) -> Request
        (True , False) -> Success
        (True , True ) -> Failure
        (False, True ) -> Indication
    c0 = testBit word 4
    c1 = testBit word 8
    method =
        (word .&. 0xf)                     -- least 4 bits remain the same
        .|. ((word .&. 0xe0)  `shiftR` 1)  -- next 3 bits are offset by 1
        .|. ((word .&. 0x3e00) `shiftR` 2) -- highest 5 bits are offset by 2

type TransactionID = (Word32, Word32, Word32)

data MessageHeader = MessageHeader { messageMethod :: Method
                                   , messageClass  :: MessageClass
                                   , messageLength :: Word16
                                   , transactionId     :: TransactionID
                                   } deriving (Eq, Show)

putMessageHeader :: MessageHeader -> PutM ()
putMessageHeader MessageHeader{..} = do
    putWord16be (encodeMessageType messageMethod messageClass)
    putWord16be messageLength
    putWord32be cookie
    let (tid1, tid2, tid3) = transactionId
    putWord32be tid1
    putWord32be tid2
    putWord32be tid3

getMessageHeader = do
    (messageMethod, messageClass) <- decodeMessageType `liftM` getWord16be
    messageLength <- getWord16be
    cookie' <- getWord32be
    guard $ cookie' == cookie
    tid1 <- getWord32be
    tid2 <- getWord32be
    tid3 <- getWord32be
    let transactionId = (tid1, tid2, tid3)
    return MessageHeader{..}

instance Serialize MessageHeader where
    put = putMessageHeader
    get = getMessageHeader

showBits a = reverse [if testBit a i then '1' else '0' | i <- [0.. bitSize a - 1]]