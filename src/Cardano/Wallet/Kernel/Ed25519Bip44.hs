{-------------------------------------------------------------------------------

  This module provides utils to perform child key derivation for bip44:

    account (parent) extended public key -> address (child) extended public key
    extended private key -> extended private key

-------------------------------------------------------------------------------}

module Cardano.Wallet.Kernel.Ed25519Bip44
    ( ChangeChain(..)

    -- key derivation functions
    , deriveAddressPublicKey
    , derivePublicKey

    -- helpers
    , isInternalChange
    ) where

import           Universum

import           Pos.Crypto (EncryptedSecretKey, PublicKey (..), encToPublic)

import           Cardano.Crypto.Wallet (DerivationScheme (DerivationScheme2),
                     deriveXPub)
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

-- | Derives address public key from the given account public key,
-- using derivation scheme 2 (please see 'cardano-crypto' package).
deriveAddressPublicKey
    :: PublicKey       -- Account Public Key
    -> ChangeChain     -- Change chain
    -> Word32          -- Address Key Index
    -> Maybe PublicKey -- Address Public Key
deriveAddressPublicKey (PublicKey accXPub) changeChain addressIx = do
    -- lvl4 derivation in bip44 is derivation of change chain
    changeXPub <- deriveXPub DerivationScheme2 accXPub (changeToIndex changeChain)
    -- lvl5 derivation in bip44 is derivation of change address chain
    PublicKey <$> deriveXPub DerivationScheme2 changeXPub addressIx

-- | Generate extend private key from extended private key
-- (EncryptedSecretKey is a wrapper around private key)
derivePublicKey :: EncryptedSecretKey -> PublicKey
derivePublicKey = encToPublic
