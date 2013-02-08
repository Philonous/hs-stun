module Network.Stun.Base where

import qualified Data.ByteString as BS
import           Data.Maybe
import           Data.Serialize
import           Data.Word

type Method = Word16

data MessageClass = Request
                  | Success
                  | Failure
                  | Indication
                    deriving (Show, Eq)

data Attribute = Attribute { attributeType :: {-# UNPACK #-} !Word16
                           , attributeValue :: BS.ByteString
                           } deriving (Show, Eq)


data TransactionID = TID {-# UNPACK #-} !Word32
                         {-# UNPACK #-} !Word32
                         {-# UNPACK #-} !Word32
                         deriving (Show, Read, Eq)

data Message = Message { messageMethod :: !Method
                       , messageClass  :: !MessageClass
                       , transactionID :: !TransactionID
                       , messageAttributes   :: [Attribute]
                       , fingerprint   :: !Bool
                       } deriving (Eq, Show)

-- | "magic cookie" constant
cookie :: Word32
cookie = 0x2112A442

data AttributeError = AttributeWrongType | AttributeDecodeError
                                           deriving (Show, Eq)

class Serialize a => IsAttribute a where
    attributeTypeValue :: a -> Word16
    toAttribute        :: a -> Attribute
    toAttribute x = Attribute { attributeType = attributeTypeValue x
                              , attributeValue = encode x
                              }
    fromAttribute      :: Attribute -> Either AttributeError a
    fromAttribute (Attribute tp vl) = x
      where x = if tp == attributeTypeValue ((\(Right r) -> r) x) then
                  case decode vl of
                      Left _  -> Left AttributeDecodeError
                      Right r -> Right r
                else Left AttributeWrongType

findAttribute :: IsAttribute a => [Attribute] -> Either AttributeError [a]
findAttribute [] = Right []
findAttribute (x:xs) = case fromAttribute x of
    Right r -> (r :) `fmap` findAttribute xs
    Left AttributeWrongType -> findAttribute xs
    Left AttributeDecodeError -> Left AttributeDecodeError
