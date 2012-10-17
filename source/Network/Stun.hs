module Network.Stun where

import Data.Word
import Control.Applicative
import System.Random

import Data.Serialize

import Network.Stun.Serialize
import Network.Stun.Base
import Network.Stun.MappedAddress

import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBS
import qualified Network.BSD as Net

bindRequest = do
    id <- (,,) <$> randomIO <*> randomIO <*> randomIO
    return $ Message { messageMethod = 1
                     , messageClass = Request
                     , transactionID = id
                     , messageAttributes = []
                     }

main = do
    s <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
    let myHints = Socket.defaultHints{Socket.addrFamily = Socket.AF_INET6}
    infos <- Socket.getAddrInfo (Just myHints) Nothing Nothing
    Socket.bind s (Socket.SockAddrInet 18667 Socket.iNADDR_ANY)
    print infos
    lh <- head . Net.hostAddresses <$> Net.getHostByName "localhost"
    let addr = Socket.SockAddrInet 3478 lh
    br <- bindRequest
    SocketBS.sendTo s (encode br) addr
    (answer, _) <- SocketBS.recvFrom s 1024
    let Right a = decode answer
    return (a :: Message)
