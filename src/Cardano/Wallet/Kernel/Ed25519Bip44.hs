{-------------------------------------------------------------------------------

  This module provides utils to perform child key derivation for bip44:

    extended private key -> extended public key
    account extended public key -> address extended public key
    account extended private key -> address extended private key

-------------------------------------------------------------------------------}

module Cardano.Wallet.Kernel.Ed25519Bip44
    ( ChangeChain(..)

    -- key derivation functions
    , deriveAddressPublicKey
    , deriveAddressPrivateKey
    , derivePublicKey

    -- helpers
    , isInternalChange
    ) where

import           Universum

import           Pos.Crypto (EncryptedSecretKey (..), PassPhrase,
                     PublicKey (..), checkPassMatches, encToPublic, isHardened)

import           Cardano.Crypto.Wallet (DerivationScheme (DerivationScheme2),
                     deriveXPrv, deriveXPub)
import           Test.QuickCheck (Arbitrary (..), elements)

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

-- | Generate extend private key from extended private key
-- (EncryptedSecretKey is a wrapper around private key)
derivePublicKey :: EncryptedSecretKey -> PublicKey
derivePublicKey = encToPublic

-- | Derives address public key from the given account public key,
-- using derivation scheme 2 (please see 'cardano-crypto' package).
deriveAddressPublicKey
    :: PublicKey       -- Account Public Key
    -> ChangeChain     -- Change chain
    -> Word32          -- Non-hardened Address Key Index
    -> Maybe PublicKey -- Address Public Key
deriveAddressPublicKey (PublicKey accXPub) changeChain addressIx = do
    -- address index should be non hardened
    guard (not $ isHardened addressIx)
    -- lvl4 derivation in bip44 is derivation of change chain
    changeXPub <- deriveXPub DerivationScheme2 accXPub (changeToIndex changeChain)
    -- lvl5 derivation in bip44 is derivation of address chain
    PublicKey <$> deriveXPub DerivationScheme2 changeXPub addressIx

-- | Derives address private key from the given account private key,
-- using derivation scheme 2 (please see 'cardano-crypto' package).
-- TODO: EncryptedSecretKey will be renamed to EncryptedPrivateKey in #164
deriveAddressPrivateKey
    :: PassPhrase               -- Passphrase used to encrypt Account Private Key
    -> EncryptedSecretKey       -- Account Private Key
    -> ChangeChain              -- Change chain
    -> Word32                   -- Non-hardened Address Key Index
    -> Maybe EncryptedSecretKey -- Address Private Key
deriveAddressPrivateKey passPhrase accEncPrvKey@(EncryptedSecretKey accXPrv passHash) changeChain addressIx = do
    -- enforce valid PassPhrase check
    checkPassMatches passPhrase accEncPrvKey
    -- address index should be non hardened
    guard (not $ isHardened addressIx)
        -- lvl4 derivation in bip44 is derivation of change chain
    let changeXPrv = deriveXPrv DerivationScheme2 passPhrase accXPrv (changeToIndex changeChain)
        -- lvl5 derivation in bip44 is derivation of address chain
        addressXPrv = deriveXPrv DerivationScheme2 passPhrase changeXPrv addressIx
    pure $ EncryptedSecretKey addressXPrv passHash
