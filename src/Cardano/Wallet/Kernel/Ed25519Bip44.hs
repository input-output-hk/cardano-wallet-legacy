{-------------------------------------------------------------------------------

  This module provides utils to perform child key derivation for bip44:

    extended private key -> extended public key
    account extended public key -> address extended public key
    master extended private key -> account extended private key

-------------------------------------------------------------------------------}

module Cardano.Wallet.Kernel.Ed25519Bip44
    ( ChangeChain(..)
    , purposeIndex
    , coinTypeIndex

    -- key derivation functions
    , deriveAddressPublicKey
    , derivePublicKey
    , deriveAccountPrivateKey

    -- helpers
    , isInternalChange
    ) where

import           Universum

import           Pos.Crypto (EncryptedSecretKey (..), PassPhrase (..),
                     PublicKey (..), checkPassMatches, encToPublic, isHardened)

import           Cardano.Crypto.Wallet (DerivationScheme (DerivationScheme2),
                     deriveXPrv, deriveXPub)
import           Test.QuickCheck (Arbitrary (..), elements)

-- | Purpose is a constant set to 44' (or 0x8000002C) following the BIP43 recommendation.
-- It indicates that the subtree of this node is used according to this specification.
--
-- Hardened derivation is used at this level.
purposeIndex :: Word32
purposeIndex = 0x8000002C

-- | One master node (seed) can be used for unlimited number of independent cryptocoins
-- such as Bitcoin, Litecoin or Namecoin. However, sharing the same space for various
-- cryptocoins has some disadvantages.
--
-- This level creates a separate subtree for every cryptocoin, avoiding reusing addresses
-- across cryptocoins and improving privacy issues.
--
-- Coin type is a constant, set for each cryptocoin. For Cardano this constant is set
-- to 1815' (or 0x80000717). 1815 is the birthyear of Ada Lovelace.
--
-- Hardened derivation is used at this level.
coinTypeIndex :: Word32
coinTypeIndex = 0x80000717

-- | Change chain. External chain is used for addresses that are meant to be visible
-- outside of the wallet (e.g. for receiving payments). Internal chain
-- is used for addresses which are not meant to be visible outside of
-- the wallet and is used for return transaction change.
data ChangeChain
    = InternalChain
    | ExternalChain
    deriving (Show, Eq)

instance Arbitrary ChangeChain where
    arbitrary = elements [InternalChain, ExternalChain]

isInternalChange :: ChangeChain -> Bool
isInternalChange InternalChain = True
isInternalChange _             = False

-- Constant 0 is used for external chain and constant 1 for
-- internal chain (also known as change addresses).
changeToIndex :: ChangeChain -> Word32
changeToIndex ExternalChain = 0
changeToIndex InternalChain = 1

-- | Derives address public key from the given account public key,
-- using derivation scheme 2 (please see 'cardano-crypto' package).
deriveAddressPublicKey
    :: PublicKey       -- Account Public Key
    -> ChangeChain     -- Change chain
    -> Word32          -- Non-hardened Address Key Index
    -> Maybe PublicKey -- Address Public Key
deriveAddressPublicKey (PublicKey accXPub) changeChain addressIx = do
    -- address index should be non-hardened
    guard (not $ isHardened addressIx)
    -- lvl4 derivation in bip44 is non-hardened derivation of change chain
    changeXPub <- deriveXPub DerivationScheme2 accXPub (changeToIndex changeChain)
    -- lvl5 derivation in bip44 is non-hardened derivation of change address chain
    PublicKey <$> deriveXPub DerivationScheme2 changeXPub addressIx

-- | Generate extend private key from extended private key
-- (EncryptedSecretKey is a wrapper around private key)
derivePublicKey :: EncryptedSecretKey -> PublicKey
derivePublicKey = encToPublic

-- | Derives account private key from the given master private key,
-- using derivation scheme 2 (please see 'cardano-crypto' package).
deriveAccountPrivateKey
    :: PassPhrase               -- Passphrase used to encrypt Master Private Key
    -> EncryptedSecretKey       -- Master Private Key
    -> Word32                   -- Hardened Account Key Index
    -> Maybe EncryptedSecretKey -- Account Private Key
deriveAccountPrivateKey passPhrase masterEncPrvKey@(EncryptedSecretKey masterXPrv passHash) accountIx = do
    -- enforce valid PassPhrase check
    checkPassMatches passPhrase masterEncPrvKey
    -- account index should be hardened
    guard (isHardened accountIx)
        -- lvl1 derivation in bip44 is hardened derivation of purpose' chain
    let purposeXPrv = deriveXPrv DerivationScheme2 passPhrase masterXPrv purposeIndex
        -- lvl2 derivation in bip44 is hardened derivation of coin_type' chain
        coinTypeXPrv = deriveXPrv DerivationScheme2 passPhrase purposeXPrv coinTypeIndex
        -- lvl3 derivation in bip44 is hardened derivation of account' chain
        accountXPrv = deriveXPrv DerivationScheme2 passPhrase coinTypeXPrv accountIx
    pure $ EncryptedSecretKey accountXPrv passHash
