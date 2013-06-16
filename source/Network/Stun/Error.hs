{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Stun.Error where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Serialize
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Network.Stun.Base

data ErrorAttribute = ErrorAttribute  { code :: {-# UNPACK #-}!Int
                                                     -- ^ Code has to be between
                                                     -- 300 and 699 inclusive
                                      , reason :: !Text -- ^ At most 128 unicode
                                                        -- characters
                                      } deriving (Show, Eq)

instance IsAttribute ErrorAttribute where
    attributeTypeValue _ = 0x0009

putErrorAttribute :: ErrorAttribute -> PutM ()
putErrorAttribute ErrorAttribute{..} = do
    putWord16be 0
    putWord8 (fromIntegral $ code `div` 100)
    putWord8 (fromIntegral $ code `mod` 100)
    putByteString $ Text.encodeUtf8 reason

getErrorAttribute :: Get ErrorAttribute
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

errTryAlternate :: ErrorAttribute
errTryAlternate = ErrorAttribute { code = 300
                                 , reason = "Try Alternate: The client should contact an alternate server for this request."
                                 }

errBadRequest :: ErrorAttribute
errBadRequest = ErrorAttribute { code = 400
                               , reason = "Bad Request: The request was malformed."

                               }

errUnauthorized :: ErrorAttribute
errUnauthorized = ErrorAttribute { code =  401
                                 , reason = "Unauthorized: The request did not contain the correct credentials to proceed."
                                 }

errUnknownAttribute :: ErrorAttribute
errUnknownAttribute = ErrorAttribute {code = 420
                                     , reason = "Unknown Attribute: The server received a STUN packet containing a comprehension-required attribute that it did not understand."
                                     }

errStaleNonce :: ErrorAttribute
errStaleNonce = ErrorAttribute {code = 438
                               , reason = "Stale Nonce: The NONCE used by the client was no longer valid."
                               }

errServerError :: ErrorAttribute
errServerError = ErrorAttribute { code = 500
                                , reason = "Server Error: The server has suffered a temporary error."
                                }
