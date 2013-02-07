{-# LANGUAGE RecordWildCards #-}
module Network.Stun.Error where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Serialize
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word

import           Network.Stun.Serialize
import           Network.Stun.Base
import qualified Data.ByteString as BS

data ErrorAttribute = ErrorAttribute  { code :: {-# UNPACK #-}!Int
                                                     -- ^ Code has to be between
                                                     -- 300 and 699 inclusive
                                      , reason :: !Text -- ^ At most 128 unicode
                                                        -- characters
                                      } deriving (Show, Eq)

instance IsAttribute ErrorAttribute where
    attributeTypeValue _ = 0x0009

putErrorAttribute ErrorAttribute{..} = do
    putWord16be 0
    putWord8 (fromIntegral $ code `div` 100)
    putWord8 (fromIntegral $ code `mod` 100)
    putByteString $ Text.encodeUtf8 reason

getErrorAttribute = do
    skip 2
    hundreds <- (fromIntegral . (0x7 .&.)) <$> getWord8 -- mask out highest 5
                                                        -- bits
    guard (hundreds >= 3 && hundreds <= 6 )
    rest <- fromIntegral <$> getWord8
    guard $ rest <= 99
    let code = 100 * hundreds + rest
    reason <- Text.decodeUtf8 <$> ensure 0
    return ErrorAttribute{..}

instance Serialize ErrorAttribute where
    put = putErrorAttribute
    get = getErrorAttribute
