{-------------------------------------------------------------------------------

  This module provides utils to perform account public key child derivation:

    account (parent) extended public key -> address (child) extended public key

-------------------------------------------------------------------------------}

module Cardano.Wallet.Kernel.Ed25519Bip44 (
    deriveLvl3PublicKey
    ) where

import           Universum

import           Pos.Crypto (PublicKey (..))

import           Cardano.Crypto.Wallet (DerivationScheme (..), deriveXPub)

-- | Derives address public key from the given account public key,
-- using derivation scheme 2 (please see 'cardano-crypto' package).
deriveLvl3PublicKey
    :: PublicKey
    -> Word32
    -> Maybe PublicKey
deriveLvl3PublicKey (PublicKey xpub) addressIx =
    PublicKey <$> deriveXPub DerivationScheme2 xpub addressIx
