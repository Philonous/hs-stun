{-# LANGUAGE RecordWildCards #-}
module Network.Stun.Error where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Serialize
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Network.Stun.Serialize
import qualified Data.ByteString as BS

data Error = Error { code :: Int  -- ^ Code has to be between 300 and 699 inclusive
                   , reason :: Text -- ^ At most 128 unicode characters
                   } deriving (Show, Eq)

putError Error{..} = do
    putWord16be 0
    putWord8 (fromIntegral $ code `div` 100)
    putWord8 (fromIntegral $ code `mod` 100)
    putByteString $ Text.encodeUtf8 reason

getError = do
    _ <- getWord16be
    hundreds <- (fromIntegral . (0x7 .&.)) <$> getWord8 -- mask out highest 5
                                                        -- bits
    guard (hundreds >= 3 && hundreds <= 6 )
    rest <- fromIntegral <$> getWord8
    guard $ rest <= 99
    let code = 100 * hundreds + rest
    reason <- Text.decodeUtf8 <$> ensure 0
    return Error{..}

instance Serialize Error where
    put = putError
    get = getError
