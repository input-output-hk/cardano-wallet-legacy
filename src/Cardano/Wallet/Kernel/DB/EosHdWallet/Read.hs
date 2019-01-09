{-# LANGUAGE RankNTypes #-}

-- | READ queries on the EOS HD wallet
--
-- NOTE: These are pure functions, which are intended to work on a snapshot
-- of the database. They are intended to support the V1 wallet API.
module Cardano.Wallet.Kernel.DB.EosHdWallet.Read (
  -- | Summarize
  addressesByAccountId
  ) where

import           Universum

import           Cardano.Wallet.Kernel.DB.EosHdWallet (EosHdWallets,
                     eosHdWalletsAddresses)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId, HdAddress)
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.DB.Util.IxSet (Indexed (..), IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet

-- | All addresses in the given account
--
-- NOTE: Does not check that the account exists.
addressesByAccountId :: HdAccountId -> Query' e EosHdWallets (IxSet (Indexed HdAddress))
addressesByAccountId accId =
    asks $ IxSet.getEQ accId . view eosHdWalletsAddresses
