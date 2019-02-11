module ByronSpecifics where

import qualified Data.HashMap.Strict as HM
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Serokell.Util.Text (listJson)
import           Universum

import           Node.Message.Class (Message (..))
import           Pos.Binary.Class (Bi (..), Cons (..), Field (..),
                     deriveSimpleBi, encodeListLen, enforceSize)
import           Pos.Chain.Block (Block, BlockHeader (..), HeaderHash)
import           Pos.Chain.Update (BlockVersion (..))
import           Pos.Core.Chrono (NE, NewestFirst (..))
import           Pos.Infra.Communication.Types.Protocol (HandlerSpec (..),
                     HandlerSpecs, VerInfo (..))
import           Pos.Util.Util (cborError)


data MsgGetHeaders = MsgGetHeaders
    { -- not guaranteed to be in any particular order
      mghFrom :: ![HeaderHash]
    , mghTo   :: !(Maybe HeaderHash)
    } deriving (Generic, Show, Eq)

instance Buildable MsgGetHeaders where
    build (MsgGetHeaders mghFrom' mghTo') =
        bprint ("MsgGetHeaders {from = "%listJson%", to = "%build%"}")
               mghFrom' (maybe "<Nothing>" (bprint build) mghTo')

instance Message MsgGetHeaders where
    messageCode _ = 4
    formatMessage _ = "GetHeaders"

deriveSimpleBi ''MsgGetHeaders [
    Cons 'MsgGetHeaders [
        Field [| mghFrom :: [HeaderHash]     |],
        Field [| mghTo   :: Maybe HeaderHash |]
    ]]

-- | 'Headers' message (see protocol specification).
data MsgHeaders
    = MsgHeaders (NewestFirst NE BlockHeader)
    | MsgNoHeaders Text
    deriving (Eq, Show, Generic)

instance Bi MsgHeaders where
    encode = \case
        MsgHeaders b -> encodeListLen 2 <> encode (0 :: Word8) <> encode b
        MsgNoHeaders t -> encodeListLen 2 <> encode (1 :: Word8) <> encode t
    decode = do
        enforceSize "MsgHeaders" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgHeaders <$> decode
            1 -> MsgNoHeaders <$> decode
            t -> cborError $ "MsgHeaders wrong tag: " <> Universum.show t

instance Message MsgHeaders where
    messageCode _ = 5
    formatMessage _ = "BlockHeaders"


-- | 'GetBlocks' message (see protocol specification).
data MsgGetBlocks = MsgGetBlocks
    { mgbFrom :: !HeaderHash
    , mgbTo   :: !HeaderHash
    } deriving (Generic, Show, Eq)

instance Buildable MsgGetBlocks where
    build (MsgGetBlocks mgbFrom' mgbTo') =
        bprint ("MsgGetBlocks {from = "%build%", to = "%build%"}")
               mgbFrom' mgbTo'

instance Message MsgGetBlocks where
    messageCode _ = 6
    formatMessage _ = "GetBlocks"

deriveSimpleBi ''MsgGetBlocks [
    Cons 'MsgGetBlocks [
        Field [| mgbFrom :: HeaderHash |],
        Field [| mgbTo   :: HeaderHash |]
    ]]

-- | 'Block' message (see protocol specification).
data MsgBlock
    = MsgBlock Block
    | MsgNoBlock Text
    deriving (Eq, Show, Generic)

instance Bi MsgBlock where
    encode = \case
        MsgBlock b -> encodeListLen 2 <> encode (0 :: Word8) <> encode b
        MsgNoBlock t -> encodeListLen 2 <> encode (1 :: Word8) <> encode t
    decode = do
        enforceSize "MsgBlock" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgBlock <$> decode
            1 -> MsgNoBlock <$> decode
            t -> cborError $ "MsgBlock wrong tag: " <> Universum.show t

instance Message MsgBlock where
    messageCode _ = 7
    formatMessage _ = "Block"



blockVersion :: BlockVersion
blockVersion = BlockVersion
               { bvMajor = 0
               , bvMinor = 0
               , bvAlt   = 0
               }

protocolMagic :: Int32
protocolMagic = 55550001

securityParameter :: Int
securityParameter = 2160

ins :: HandlerSpecs
ins = HM.fromList [(96,ConvHandler 67),(49,ConvHandler 92),(97,ConvHandler 61),(98,ConvHandler 55),(67,ConvHandler 96),(83,ConvHandler 0),(4,ConvHandler 5),(5,ConvHandler 4),(37,ConvHandler 94),(6,ConvHandler 7),(55,ConvHandler 98),(73,ConvHandler 95),(43,ConvHandler 93),(92,ConvHandler 49),(13,ConvHandler 0),(61,ConvHandler 97),(93,ConvHandler 43),(14,ConvHandler 0),(94,ConvHandler 37),(15,ConvHandler 16),(95,ConvHandler 73)]

outs :: HandlerSpecs
outs = HM.fromList [(49,ConvHandler 92),(67,ConvHandler 96),(83,ConvHandler 0),(4,ConvHandler 5),(5,ConvHandler 4),(37,ConvHandler 94),(6,ConvHandler 7),(55,ConvHandler 98),(73,ConvHandler 95),(43,ConvHandler 93),(13,ConvHandler 0),(61,ConvHandler 97),(14,ConvHandler 0),(15,ConvHandler 16)]

usedVerInfo :: VerInfo
usedVerInfo = VerInfo protocolMagic blockVersion ins outs
