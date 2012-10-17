module Network.Stun.MappedAddress where

import Control.Applicative ((<$>))
import Control.Monad

import Data.Bits
import Data.Serialize
import Data.Word

import Network
import Network.Endian
import Network.Socket
import Network.Stun.Base

import Data.Serialize

xmaAttributeType = 0x0020

data Address = Inet  PortNumber HostAddress
             | Inet6 PortNumber HostAddress6
               deriving (Eq, Show)

-- most significant 16 bits of the magic cookie
halfCookie :: Word16
halfCookie = fromIntegral $ cookie `shiftR` 16

putAddress (Inet port address) = do
    putWord8 0
    putWord8 1
    putWord16be $ fromIntegral port
    putWord32host address -- address already is BE
putAddress (Inet6 port (addr1, addr2, addr3, addr4)) = do
    putWord8 0
    putWord8 2
    putWord16be $ fromIntegral port
    putWord32be addr4
    putWord32be addr3
    putWord32be addr2
    putWord32be addr1

getAddress = do
    guard . (== 0) =<< getWord8
    family <- getWord8
    port <- fromIntegral <$> getWord16be
    case family of
        1 -> do
            address <- getWord32host
            return $ Inet port address
        2 -> do
            addr4 <- getWord32be
            addr3 <- getWord32be
            addr2 <- getWord32be
            addr1 <- getWord32be
            return (Inet6 port (addr1, addr2, addr3, addr4))
        _ -> mzero

xorAddress _ (Inet port address) =
    Inet (fromIntegral (halfCookie `xor` (fromIntegral port)))
         (htonl cookie `xor` address)
xorAddress (tid1, tid2, tid3) (Inet6 port (addr1, addr2, addr3, addr4)) =
    Inet6 (fromIntegral (halfCookie `xor` (fromIntegral port)))
          ( cookie `xor` addr1
          , tid1   `xor` addr2
          , tid2   `xor` addr3
          , tid3   `xor` addr4
          )

instance Serialize Address where
    put = putAddress
    get = getAddress