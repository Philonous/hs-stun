module Network.Stun where

import Data.Word
import Data.Digest.CRC32
import Data.List(find)
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Timeout
import Control.Monad.IO.Class
import Control.Monad.Trans.Error
import System.Random

import Data.Serialize
import qualified Data.ByteString as BS

import Foreign
import Foreign.C

import Network.Stun.Serialize
import Network.Stun.Base
import Network.Stun.MappedAddress
import Network.Stun.Error

import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SocketBS
import qualified Network.BSD as Net

-- | Generate a new bind request
bindRequest :: IO Message
bindRequest = do
    id <- TID <$> randomIO <*> randomIO <*> randomIO
    return $ Message { messageMethod = 1
                     , messageClass = Request
                     , transactionID = id
                     , messageAttributes = []
                     , fingerprint = True
                     }

data StunError = TimeOut
               | ProtocolError -- !BS.ByteString
               | ErrorMsg !Message
               | WrongMessageType !Message
  deriving (Show, Eq)
instance Error StunError

-- | Send a STUN request to the server denoted by address and wait for an
-- answer. The request will be sucessively sent once for each element of
-- timeOuts until an answer is received or all requests time out.
stunRequest
  :: S.SockAddr     -- ^ Address of the stun server
  -> Net.PortNumber -- ^ local port to use
  -> [Integer]      -- ^ time outs in µs (10^-6 seconds), will default to
                    -- [0.5s,  1s, 2s] if empty. 0 means wait indefinitly.
  -> Message        -- ^ Request to send
  -> IO (Either StunError Message)
stunRequest host localPort timeOuts msg = runErrorT $ do
    (r, s) <- ErrorT $ stunRequest' host localPort timeOuts msg
    liftIO $ S.close s
    return r


-- | Same as 'stunRequest' but returns the used socket
stunRequest'
  :: S.SockAddr     -- ^ Address of the stun server
  -> Net.PortNumber -- ^ local port to use
  -> [Integer]      -- ^ time outs in µs (10^-6 seconds), will default to
                    -- [0.5s,  1s, 2s] if empty. 0 means wait indefinitly.
  -> Message        -- ^ Request to send
  -> IO (Either StunError (Message, S.Socket))
stunRequest' host' localPort timeOuts msg = runErrorT $ do
    let host  = setHostPort host'
    s <- liftIO $ case host of
        S.SockAddrInet hostPort ha ->  do
            s <- S.socket S.AF_INET S.Datagram S.defaultProtocol
            return s
        S.SockAddrInet6 hostPort fi ha sid -> do
            s <- S.socket S.AF_INET6 S.Datagram S.defaultProtocol
            S.setSocketOption s S.IPv6Only 1
            return s
    liftIO $ S.connect s host
    let go [] = liftIO (S.close s) >> throwError TimeOut
        go (to:tos) = do
            liftIO $ SocketBS.sendTo s (encode msg) host
            r <- liftIO . timeout to $ SocketBS.recvFrom s 1024
            case r of
                Nothing -> go tos
                Just (answer, _) -> return answer
    answer <- go $ if null timeOuts then [500000, 1000000, 2000000] else timeOuts
    case decode answer of
        Left _ -> throwError $ ProtocolError -- answer
        Right msg -> do
            case messageClass msg of
                Failure -> throwError $ ErrorMsg msg
                Success -> return (msg, s)
                _ -> throwError $ WrongMessageType msg
  where
    setHostPort (S.SockAddrInet pn ha) = S.SockAddrInet
                                         (if pn == 0 then 3478 else pn) ha
    setHostPort (S.SockAddrInet6 pn fl ha si) = S.SockAddrInet6
                                                (if pn == 0 then 3478 else pn)
                                                fl ha si
    setHostPort s = s

-- | Get the mapped address by sending a bind request to /host/, using
-- /localport/. The request will be retransmitted for each entry of /timeOuts/.
-- If the list of time outs is empty, a default of 500ms, 1s and 2s is used
-- returns the reflexive and the local address
findMappedAddress :: S.SockAddr -- ^ STUN server address
                  -> Net.PortNumber -- ^ local port to use
                  -> [Integer] -- ^ timeOuts in µs (10^-6 seconds)
                  -> IO (Either StunError (S.SockAddr, S.SockAddr))
findMappedAddress host localPort timeOuts = runErrorT $ do
    br <- liftIO $ bindRequest
    (msg, s) <- ErrorT $ stunRequest' host localPort timeOuts br
    let xma' = find ((== xmaAttributeType) . attributeType ) $ messageAttributes msg
    xma <- case xma' of
        Nothing -> return Nothing
        Just xma'' -> case decode . attributeValue $ xma'' of
            Left e -> throwError $ ProtocolError -- answer
            Right r -> return . Just $! xorAddress (transactionID msg) r
    let ma' = find ((== maAttributeType) . attributeType ) $ messageAttributes msg
    ma <- case ma' of
        Nothing -> return Nothing
        Just ma'' -> case decode . attributeValue $ ma'' of
            Left e -> throwError $ ProtocolError -- answer
            Right r -> return . Just $! r
    m <- case (xma <|> ma) of
        Just m' -> return m'
        Nothing -> throwError $ ProtocolError -- no mapped Address
    local <- liftIO $ S.getSocketName s
    liftIO $ S.sClose  s
    return $ (unMA m, local)
