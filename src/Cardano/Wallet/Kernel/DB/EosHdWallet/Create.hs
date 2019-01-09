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

import           Pos.Crypto (PublicKey)

import           Cardano.Wallet.Kernel.DB.EosHdWallet
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.EosWalletId (EosWalletId)

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Errors thrown by 'createEosHdWallet'
data CreateEosHdRootError =
    -- | We already have an EOS-wallet with the specified ID
    CreateEosHdRootExists EosWalletId

-- | Errors thrown by 'createEosHdAccount'
data CreateEosHdAccountError =
    -- | The specified EOS-wallet could not be found
    CreateEosHdAccountUnknownRoot UnknownEosHdRoot
    -- | Account already exists
  | CreateEosHdAccountExists PublicKey

-- | Errors thrown by 'createEosHdAddress'
data CreateEosHdAddressError =
    -- | Account not found
    CreateEosHdAddressUnknown UnknownHdAccount
    -- | Address already used
  | CreateEosHdAddressExists HdAddressId

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
    zoomEosWalletId CreateEosHdAccountUnknownRoot rootId $
      return ()

    zoom eosHdWalletsAccounts $ do
      exists <- gets $ IxSet.member accountPK
      when exists $ throwError $ CreateEosHdAccountExists accountPK
      at accountPK .= Just eosHdAccount
  where
    accountPK = eosHdAccount ^. eosHdAccountPK
    rootId    = eosHdAccount ^. eosHdAccountRootId

-- | Create a new address in account in EOS-wallet.
createEosHdAddress :: HdAddress -> Update' CreateEosHdAddressError EosHdWallets ()
createEosHdAddress hdAddress = do
    -- Create the new address
    zoom eosHdWalletsAddresses $ do
        exists <- gets $ IxSet.member addrId
        when exists $ throwError $ CreateEosHdAddressExists addrId
        -- We don't have 'AutoIncrementKey' for 'EosHdAccount', but since we use
        -- the same type hierarchy as for 'HdAddress', we have to provide fake
        -- 'AutoIncrementKey' value.
        let fakeAutoIncrementKey = IxSet.AutoIncrementKey 0
        at addrId .= Just (IxSet.Indexed fakeAutoIncrementKey hdAddress)
  where
    addrId = hdAddress ^. hdAddressId

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
