{-# LANGUAGE RankNTypes #-}

-- | CREATE operations on externally-owned sequential (EOS) HD wallets
module Cardano.Wallet.Kernel.DB.EosHdWallet.Create (
    -- * Errors
    CreateEosHdRootError(..)
  , CreateEosHdAccountError(..)
  , CreateEosHdAddressError(..)
    -- * Functions
  , createEosHdRoot
  , createEosHdAccount
  , createEosHdAddress
  ) where

import           Universum

import           Control.Lens (at, (.=))
import           Data.SafeCopy (base, deriveSafeCopy)

import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable

import           Cardano.Wallet.Kernel.DB.EosHdWallet
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Errors thrown by 'createEosHdWallet'
data CreateEosHdRootError =
    -- | We already have an EOS-wallet with the specified ID
    CreateEosHdRootExists EosHdRootId

-- | Errors thrown by 'createEosHdAccount'
data CreateEosHdAccountError =
    -- | The specified EOS-wallet could not be found
    CreateEosHdAccountUnknownRoot UnknownEosHdRoot
    -- | Account already exists
  | CreateEosHdAccountExists EosHdAccountId

-- | Errors thrown by 'createEosHdAddress'
data CreateEosHdAddressError =
    -- | Account not found
    CreateEosHdAddressUnknown UnknownHdAccount
    -- | Address already used
  | CreateEosHdAddressExists EosHdAddressId

deriveSafeCopy 1 'base ''CreateEosHdRootError
deriveSafeCopy 1 'base ''CreateEosHdAccountError
deriveSafeCopy 1 'base ''CreateEosHdAddressError

{-------------------------------------------------------------------------------
  CREATE
-------------------------------------------------------------------------------}

-- | Create a new EOS-wallet.
createEosHdRoot :: EosHdRoot -> Update' CreateEosHdRootError EosHdWallets ()
createEosHdRoot eosHdRoot = do
    zoom eosHdWalletsRoots $ do
      exists <- gets $ IxSet.member rootId
      when exists $ throwError $ CreateEosHdRootExists rootId
      at rootId .= Just eosHdRoot
  where
    rootId = eosHdRoot ^. eosHdRootId

-- | Create a new account in EOS-wallet
createEosHdAccount :: EosHdAccount -> Update' CreateEosHdAccountError EosHdWallets ()
createEosHdAccount eosHdAccount = do
    -- Check that the root ID exists
    zoomEosHdRootId CreateEosHdAccountUnknownRoot rootId $
      return ()

    zoom eosHdWalletsAccounts $ do
      exists <- gets $ IxSet.member accountId
      when exists $ throwError $ CreateEosHdAccountExists accountId
      at accountId .= Just eosHdAccount
  where
    accountId = eosHdAccount ^. eosHdAccountId
    rootId    = eosHdAccount ^. eosHdAccountRootId

-- | Create a new address in account in EOS-account
createEosHdAddress :: EosHdAddress -> Update' CreateEosHdAddressError EosHdWallets ()
createEosHdAddress eosHdAddress = do
    zoom eosHdWalletsAddresses $ do
      exists <- gets $ IxSet.member addrId
      when exists $ throwError $ CreateEosHdAddressExists addrId
      at addrId .= Just eosHdAddress
  where
    addrId = eosHdAddress ^. eosHdAddressId

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

instance Buildable CreateEosHdRootError where
    build (CreateEosHdRootExists rootId)
        = bprint ("CreateEosHdRootError::CreateEosHdRootExists "%build) rootId

instance Buildable CreateEosHdAccountError where
    build (CreateEosHdAccountUnknownRoot (UnknownEosHdRoot rootId))
        = bprint ("CreateHdAccountError::CreateHdAccountUnknownRoot "%build) rootId
    build (CreateEosHdAccountExists accountId)
        = bprint ("CreateEosHdAccountError::CreateEosHdAccountExists "%build) accountId

instance Buildable CreateEosHdAddressError where
    build (CreateEosHdAddressUnknown (UnknownHdAccount accId))
        = bprint ("CreateEosHdAddressError::CreateEosHdAddressUnknown UnknownHdAccount "%build) accId
    build (CreateEosHdAddressUnknown (UnknownHdAccountRoot accId))
        = bprint ("CreateEosHdAddressError::CreateEosHdAddressUnknown UnknownHdAccount "%build) accId
    build (CreateEosHdAddressExists addrId)
        = bprint ("CreateEosHdAddressError::CreateEosHdAddressExists "%build) addrId
