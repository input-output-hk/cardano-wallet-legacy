{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Wallet.Kernel.DB.Sqlite.Persistent.Orphans where

import           Universum hiding (Read)

import           Prelude (Read (..))

import           Data.Time.Units (fromMicroseconds, toMicroseconds)
import           Database.Persist.Sqlite
import           Formatting (sformat)

import qualified Pos.Chain.Txp as Txp
import qualified Pos.Core as Core
import           Pos.Crypto.Hashing (decodeAbstractHash, hashHexF)

instance PersistField Txp.TxId where
    toPersistValue =
        toPersistValue . sformat hashHexF
    fromPersistValue pv = do
        h <- decodeAbstractHash <$> fromPersistValue pv
        case h of
             Left _     ->
                 Left
                 $ "not a valid hex hash while decoding TxId: "
                 <> show pv
             Right txid ->
                 pure txid

instance PersistFieldSql Txp.TxId where
    sqlType _ = sqlType (Proxy @Text)

instance PersistField Core.Coin where
    toPersistValue =
        toPersistValue . Core.unsafeGetCoin
    fromPersistValue =
        fmap Core.Coin . fromPersistValue

instance PersistFieldSql Core.Coin where
    sqlType _ = sqlType (Proxy @Word64)

-- NOTE(adn) As reported by our good lad Matt Parsons, 'Word64' has enough
-- precision to justify the downcast:
--
-- >>> λ> import Data.Time
-- >>> λ> import Data.Time.Clock.POSIX
-- >>> λ> import Data.Word
-- >>> λ> :set -XNumDecimals
-- >>> λ> posixSecondsToUTCTime (fromIntegral ((maxBound :: Word64) `div` 1e6))
-- 586524-01-19 08:01:49 UTC
instance PersistField Core.Timestamp where
    toPersistValue =
        toPersistValue
        . fromIntegral @Integer @Word64
        . toMicroseconds
        . Core.getTimestamp
    fromPersistValue f =
        Core.Timestamp . fromMicroseconds . toInteger @Word64 <$> fromPersistValue f

instance PersistFieldSql Core.Timestamp where
    sqlType _ = sqlType (Proxy @Word64)

instance PersistField Core.Address where
    toPersistValue =
        toPersistValue . sformat Core.addressF
    fromPersistValue f = do
        addr <- Core.decodeTextAddress <$> fromPersistValue f
        case addr of
           Left _  -> Left $ "Failed to parse Address out of PersistValue: " <> show f
           Right a -> pure a

instance PersistField HdRootId where
    toPersistValue = toPersistValue . sformat build
    fromPersistValue f = do
        rootId <- decodeHdRootId <$> fromPersistValue f
        case rootId of
           Nothing -> returnError Sqlite.ConversionFailed f "not a valid HdRootId"
           Just a -> pure a

instance Read Core.Address where
    readsPrec _ = error "Needed for HttpApiData"

instance PersistFieldSql Core.Address where
    sqlType _ = sqlType (Proxy @Text)
