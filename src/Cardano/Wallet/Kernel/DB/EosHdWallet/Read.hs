{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

-- | READ queries on the EOS HD wallet
--
-- NOTE: These are pure functions, which are intended to work on a snapshot
-- of the database. They are intended to support the V1 wallet API.
module Cardano.Wallet.Kernel.DB.EosHdWallet.Read (
    -- | Summarize
    addressesByAccountId
  , accountPublicKey
  ) where

import           Universum

import           Pos.Crypto (PublicKey)

import           Cardano.Wallet.Kernel.DB.EosHdWallet
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet

{-------------------------------------------------------------------------------
  Summarize
-------------------------------------------------------------------------------}

-- | All addresses in the given account
--
-- NOTE: Does not check that the account exists.
addressesByAccountId :: EosHdAccountId -> Query' e EosHdWallets (IxSet EosHdAddress)
addressesByAccountId accId =
    asks $ IxSet.getEQ accId . view eosHdWalletsAddresses

-- | Public key for the given account.
accountPublicKey :: EosHdAccountId -> Query' UnknownEosHdAccount EosHdWallets PublicKey
accountPublicKey accId =
    zoomEosHdAccountId identity accId $ view eosHdAccountPK
