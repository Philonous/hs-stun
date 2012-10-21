{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Stun.Serialize where

import           Control.Monad
import           Data.Bits
import           Data.Serialize
import           Data.Word
import           Data.Digest.CRC32
import           Network
import           Network.Stun.Base
import           Numeric

import Debug.Trace

import qualified Data.ByteString as BS

putAttribute Attribute{..} = do
    putWord16be attributeType
    putWord16be (fromIntegral $ BS.length attributeValue)
    putByteString attributeValue
    -- padding:
    replicateM (negate (BS.length attributeValue) `mod` 4) $ putWord8 0
    return ()

getAttribute = do
    attributeType <- getWord16be
    length <- getWord16be
    attributeValue <- getBytes (fromIntegral length)
    -- consume padding:
    _ <- replicateM (negate (fromIntegral length) `mod` 4) $ getWord8
    return Attribute{..}

instance Serialize Attribute where
    put = putAttribute
    get = getAttribute

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


fingerprintXorConstant :: Word32
fingerprintXorConstant = 0x5354554e

fingerprintAttribute :: Word32 -> Attribute
fingerprintAttribute crc = Attribute { attributeType = 0x8028
                            , attributeValue = encode $ crc `xor` fingerprintXorConstant
                            }

putPlainMessage :: Int -> Message -> PutM ()
putPlainMessage plusSize m@Message{..} = do
    putWord16be (encodeMessageType messageMethod messageClass)
    let messageBody = runPut . void $ mapM put messageAttributes
    let messageLength = (fromIntegral $ BS.length messageBody + plusSize)
    putWord16be messageLength
    putWord32be cookie
    let (tid1, tid2, tid3) = transactionID
    putWord32be tid1
    putWord32be tid2
    putWord32be tid3
    putByteString messageBody

putMessage m | fingerprint m = do
    let msg = runPut $ putPlainMessage 8 m
    putByteString msg
    put . fingerprintAttribute . crc32 $ msg
             | otherwise = putPlainMessage 0 m

getMessage = do
    (mlen, msg) <- lookAhead $ do
        tp <- getWord16be
        guard $ 0xc000 .&. tp == 0 -- highest 2 bits are 0
        let (messageMethod, messageClass) = decodeMessageType tp
        messageLength <- fromIntegral `fmap` getWord16be
        guard $ messageLength `mod` 4 == 0
        guard . (== cookie) =<< getWord32be
        transactionID <- liftM3 (,,) getWord32be getWord32be getWord32be
        messageAttributes <- getMessageAttributes
        let fingerprint = False
        return (messageLength, Message{..})
    case reverse . messageAttributes $ msg of
        (Attribute 0x8028 fp :_) -> do
            start <- getBytes ( 20 -- header length
                              + mlen - 8)
            let crc = fingerprintXorConstant `xor` crc32 start
            label "fingeprint does not match" $ guard (encode crc == fp)
            return msg{ fingerprint = encode crc == fp
                      , messageAttributes = init . messageAttributes $ msg
                      }
        _ -> return msg
  where
    getMessageAttributes = isEmpty >>= \e -> if e then return [] else go
    go = do
        attr <- getAttribute
        empty <- isEmpty
        rest <- if empty then return [] else go
        return $ attr:rest


instance Serialize Message where
    put = putMessage
    get = getMessage

showBits a = reverse [if testBit a i then '1' else '0' | i <- [0.. bitSize a - 1]]
