module Network.Stun.Credentials
    ( Credentials(..)
    , Username(..)
    , MessageIntegrity(..)
    , withMessageIntegrity
    , checkMessageIntegrity
    ) where

import           Control.Monad
import           Crypto.HMAC
import qualified Crypto.Hash.CryptoAPI as Crypto
import qualified Crypto.Classes as Crypto
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (fromChunks)
import           Data.List
import           Data.Serialize
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Network.Stun.Base
import           Network.Stun.Serialize

data Username = Username {unUsername :: !Text}

instance Serialize Username where
    put = putByteString . Text.encodeUtf8 . unUsername
    get = (Username . Text.decodeUtf8) `liftM` ensure 0

instance IsAttribute Username where
    attributeTypeValue _ = 0x0006

data Credentials = LongTerm !Text !Text !Text -- ^ username realm password
                 | ShortTerm !Text !Text -- ^ username password

cUsername :: Credentials -> Text
cUsername (LongTerm uname _ _) = uname
cUsername (ShortTerm uname _) = uname

data MessageIntegrity = MessageIntegrity { miHmac :: !ByteString}
                          deriving (Show, Eq)

instance Serialize MessageIntegrity where
    put = putByteString . miHmac
    get = MessageIntegrity `fmap` ensure 20

instance IsAttribute MessageIntegrity where
    attributeTypeValue _ = 0x0008

mkMessageIntegrity :: Credentials -> Message -> MessageIntegrity
mkMessageIntegrity cred m = let
    msg = runPut $ putPlainMessage 24 m
    key = case cred of
        LongTerm uname realm pwd -> MacKey . md5hash
                                    . Text.encodeUtf8
                                    . Text.intercalate (Text.singleton ':') $
                                    [ uname
                                    , realm
                                    , pwd -- TODO: SaslPrep
                                    ]
        ShortTerm _ pwd -> MacKey $ Text.encodeUtf8 pwd --TODO: SaslPrep
    mac :: Crypto.SHA1
    mac = hmac key $ fromChunks [msg]
    in MessageIntegrity $ encode mac
  where
    md5hash :: ByteString -> ByteString
    md5hash = Crypto.encode . (Crypto.hash' :: ByteString -> Crypto.MD5)

-- | Generate a MESSAGE-INTEGRITY attribute and append it to the message
-- attribute list
withMessageIntegrity :: Credentials -> Message -> Message
withMessageIntegrity cred msg = msg{messageAttributes =
                                         messageAttributes msg ++ [uname, integrity]
                                   }
  where
    uname = toAttribute $ Username (cUsername cred)
    integrity = toAttribute $
                mkMessageIntegrity cred msg{messageAttributes =
                                                 messageAttributes msg ++ [uname]}


-- | Checks the credentials of a message
--
-- * returns Nothing when the credentials don't match
--
-- * returns Just (False, oldmsg) when no MESSAGE-INTEGRITY attribute is present
--
-- where oldmsg is the unchanged message passed to the function
--
-- * returns Just (True, prunedmsg) when the attribute is present and matches
--
-- where prunedmsg is the message with all fields after MESSAGE-INTEGRITY removed
checkMessageIntegrity :: Credentials -> Message -> Maybe (Bool, Message)
checkMessageIntegrity cred msg = let
    (attrs, rest) = break ((==)(attributeTypeValue (undefined :: MessageIntegrity))
                       . attributeType ) $ messageAttributes msg
    in case rest of
        [] -> Just (False, msg)
        (inte: _) -> if fromAttribute inte
                       == Right (mkMessageIntegrity cred
                                   msg{messageAttributes = attrs})
                       then Just (True, msg {messageAttributes = attrs})
                     else Nothing
