module Network.Stun.MappedAddress where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Bits
import Data.Serialize
import Data.Word
import Network.Socket
import Network.Stun.Base

xmaAttributeType :: Word16
xmaAttributeType = 0x0020

maAttributeType :: Word16
maAttributeType = 0x0001

newtype MappedAddress = MA{unMA :: SockAddr }
               deriving (Eq, Show)

instance Serialize MappedAddress where
    put = putAddress . unMA
    get = MA `liftM` getAddress

instance IsAttribute MappedAddress where
    attributeTypeValue _ = 0x0001


newtype XorMappedAddress = XMA{unXMA :: SockAddr }
               deriving (Eq, Show)

instance Serialize XorMappedAddress where
    put = putAddress . unXMA
    get = XMA `liftM` getAddress

instance IsAttribute XorMappedAddress where
    attributeTypeValue _ = 0x0020


-- most significant 16 bits of the magic cookie
halfCookie :: Word16
halfCookie = fromIntegral $ cookie `shiftR` 16

putAddress :: SockAddr -> PutM ()
putAddress (SockAddrInet port address) = do
    putWord8 0
    putWord8 1
    putWord16be $ fromIntegral port
    putWord32host address -- address already is BE
putAddress (SockAddrInet6 port _ (addr1, addr2, addr3, addr4) _ ) = do
    putWord8 0
    putWord8 2
    putWord16be $ fromIntegral port
    putWord32be addr1
    putWord32be addr2
    putWord32be addr3
    putWord32be addr4
putAddress _ = error "putAddress: Address type not implemented"

getAddress :: Get SockAddr
getAddress = do
    guard . (== 0) =<< getWord8
    family <- getWord8
    port <- fromIntegral <$> getWord16be
    case family of
        1 -> do
            address <- getWord32host
            return $ SockAddrInet port address
        2 -> do
            addr1 <- getWord32be
            addr2 <- getWord32be
            addr3 <- getWord32be
            addr4 <- getWord32be
            return $ (SockAddrInet6 port 0 (addr1, addr2, addr3, addr4)) 0
        _ -> mzero

fromXorMappedAddress :: TransactionID -> XorMappedAddress -> SockAddr
fromXorMappedAddress tid (XMA addr) = xorAddress tid addr

xorMappedAddress :: TransactionID -> SockAddr -> XorMappedAddress
xorMappedAddress tid addr = XMA $ xorAddress tid addr

xorAddress :: TransactionID -> SockAddr -> SockAddr
xorAddress _ (SockAddrInet port address) =
    SockAddrInet (fromIntegral (halfCookie `xor` (fromIntegral port)))
                      (htonl cookie `xor` address)
xorAddress (TID tid1 tid2 tid3)
           (SockAddrInet6 port fi (addr1, addr2, addr3, addr4) sid) =
    SockAddrInet6 (fromIntegral (halfCookie `xor` (fromIntegral port)))
          fi
          ( cookie `xor` addr1
          , tid1   `xor` addr2
          , tid2   `xor` addr3
          , tid3   `xor` addr4
          )
          sid
xorAddress _ _ = error "xorAddress does not work on SockAddrUnix"
