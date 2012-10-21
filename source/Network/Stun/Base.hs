module Network.Stun.Base where

import qualified Data.ByteString as BS
import           Data.Word

type Method = Word16

data MessageClass = Request
                  | Success
                  | Failure
                  | Indication
                    deriving (Show, Eq)

data Attribute = Attribute { attributeType :: Word16
                           , attributeValue :: BS.ByteString
                           } deriving (Show, Eq)


type TransactionID = (Word32, Word32, Word32)

data Message = Message { messageMethod :: Method
                       , messageClass  :: MessageClass
                       , transactionID :: TransactionID
                       , messageAttributes   :: [Attribute]
                       , fingerprint    :: Bool
                       } deriving (Eq, Show)

-- "magic cookie" constant
cookie :: Word32
cookie = 0x2112A442
