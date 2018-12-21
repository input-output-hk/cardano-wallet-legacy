module Cardano.Wallet.Kernel.DB.Read (
    -- | Getters across the entire kernel
    walletIds
    -- | Summarize
  , accountsByRootId
  , addressesByRootId
  , addressesByAccountId
  , eosAddressesByAccountId
  , eosAccountPublicKey
  , pendingByAccount
  , foreignPendingByAccount
    -- | Lookups
  , lookupHdRootId
  , lookupHdAccountId
  , lookupHdAddressId
  , lookupCardanoAddress
    -- | Properties of an entire root
  , rootAssuranceLevel
  , rootTotalBalance
    -- | Queries on an account's current checkpoint
  , currentUtxo
  , currentAvailableUtxo
  , currentTotalBalance
  , currentAvailableBalance
  , currentAddressMeta
  , currentTxSlotId
  , currentTxIsPending
  ) where

import           Universum

import           Pos.Chain.Txp (TxId, Utxo)
import           Pos.Core (Address, Coin, SlotId)
import           Pos.Crypto (PublicKey)

import           Cardano.Wallet.Kernel.DB.AcidState (DB, dbEosHdWallets,
                     dbHdWallets)
import           Cardano.Wallet.Kernel.DB.BlockMeta (AddressMeta)
import           Cardano.Wallet.Kernel.DB.EosHdWallet
import qualified Cardano.Wallet.Kernel.DB.EosHdWallet.Read as EosHD
import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Read as HD
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.DB.Util.IxSet (Indexed, IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet

{-------------------------------------------------------------------------------
  Getters across the entire kernel
-------------------------------------------------------------------------------}

walletIds :: DB -> [HdRootId]
walletIds db = map (view hdRootId)
             $ IxSet.toList
             $ db ^. dbHdWallets . hdWalletsRoots

{-------------------------------------------------------------------------------
  Lift functions from "Cardano.Wallet.Kernel.DB.Spec.Read"

  NOTE: Right now this may seem pretty pointless. The idea is that in the
  future we might have other kinds of wallets; the various submodules of
  ("Cardano.Wallet.Kernel.DB.SomeWalletType.Read") should then be unified
  here. Right now we just wrap and make no effort to unify the types.
-------------------------------------------------------------------------------}

accountsByRootId :: DB -> HdRootId -> IxSet HdAccount
accountsByRootId = liftNoErrorsHd1 HD.accountsByRootId

-- | All addresses in the given wallet
addressesByRootId :: DB -> HdRootId -> IxSet (Indexed HdAddress)
addressesByRootId = liftNoErrorsHd1 HD.addressesByRootId

-- | All addresses in the given account
addressesByAccountId :: DB -> HdAccountId -> IxSet (Indexed HdAddress)
addressesByAccountId = liftNoErrorsHd1 HD.addressesByAccountId

-- | All addresses in the given account (in EOS-wallet)
eosAddressesByAccountId :: DB -> EosHdAccountId -> IxSet EosHdAddress
eosAddressesByAccountId = liftNoErrorsEosHd1 EosHD.addressesByAccountId

pendingByAccount :: DB -> Map HdAccountId Pending
pendingByAccount = liftNoErrorsHd0 HD.pendingByAccount

foreignPendingByAccount :: DB -> Map HdAccountId Pending
foreignPendingByAccount = liftNoErrorsHd0 HD.foreignPendingByAccount

lookupHdRootId :: DB -> HdRootId -> Either UnknownHdRoot HdRoot
lookupHdRootId = liftHd1 HD.lookupHdRootId

lookupHdAccountId :: DB -> HdAccountId -> Either UnknownHdAccount HdAccount
lookupHdAccountId = liftHd1 HD.lookupHdAccountId

lookupHdAddressId :: DB -> HdAddressId -> Either UnknownHdAddress HdAddress
lookupHdAddressId = liftHd1 HD.lookupHdAddressId

lookupCardanoAddress :: DB -> Address -> Either UnknownHdAddress HdAddress
lookupCardanoAddress = liftHd1 HD.lookupCardanoAddress

rootAssuranceLevel :: DB -> HdRootId -> Either UnknownHdRoot AssuranceLevel
rootAssuranceLevel = liftHd1 HD.rootAssuranceLevel

-- | We store public key for each account in EOS-wallet (we receive these
-- public keys during EOS-wallet creation).
eosAccountPublicKey :: DB -> EosHdAccountId -> Either UnknownEosHdAccount PublicKey
eosAccountPublicKey = liftEosHd1 EosHD.accountPublicKey

rootTotalBalance :: DB -> HdRootId -> Coin
rootTotalBalance = liftNoErrorsHd1 HD.rootTotalBalance

currentUtxo :: DB -> HdAccountId -> Either UnknownHdAccount Utxo
currentUtxo = liftHd1 HD.currentUtxo

currentAvailableUtxo :: DB -> HdAccountId -> Either UnknownHdAccount Utxo
currentAvailableUtxo = liftHd1 HD.currentAvailableUtxo

currentTotalBalance :: DB -> HdAccountId -> Either UnknownHdAccount Coin
currentTotalBalance = liftHd1 HD.currentTotalBalance

currentAvailableBalance :: DB -> HdAccountId -> Either UnknownHdAccount Coin
currentAvailableBalance = liftHd1 HD.currentAvailableBalance

currentAddressMeta :: DB -> HdAddress -> Either UnknownHdAccount AddressMeta
currentAddressMeta = liftHd1 HD.currentAddressMeta

currentTxSlotId :: DB -> TxId -> HdAccountId -> Either UnknownHdAccount (CombinedWithAccountState (Maybe SlotId))
currentTxSlotId = liftHd2 HD.currentTxSlotId

currentTxIsPending :: DB -> TxId -> HdAccountId -> Either UnknownHdAccount Bool
currentTxIsPending = liftHd2 HD.currentTxIsPending

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

liftHd0 :: Query' err HdWallets z -> DB -> Either err z
liftHd0 f = runQuery' f . view dbHdWallets

liftEosHd0 :: Query' err EosHdWallets z -> DB -> Either err z
liftEosHd0 f = runQuery' f . view dbEosHdWallets

liftHd1 :: (a -> Query' err HdWallets z) -> DB -> a -> Either err z
liftHd1 f db a = liftHd0 (f a) db

liftEosHd1 :: (a -> Query' err EosHdWallets z) -> DB -> a -> Either err z
liftEosHd1 f db a = liftEosHd0 (f a) db

liftHd2 :: (a -> b -> Query' err HdWallets z) -> DB -> a -> b -> Either err z
liftHd2 f db a b = liftHd0 (f a b) db

liftNoErrorsHd0 :: Query' Void HdWallets z -> DB -> z
liftNoErrorsHd0 f = runQueryNoErrors f . view dbHdWallets

liftNoErrorsEosHd0 :: Query' Void EosHdWallets z -> DB -> z
liftNoErrorsEosHd0 f = runQueryNoErrors f . view dbEosHdWallets

liftNoErrorsHd1 :: (a -> Query' Void HdWallets z) -> DB -> a -> z
liftNoErrorsHd1 f db a = liftNoErrorsHd0 (f a) db

liftNoErrorsEosHd1 :: (a -> Query' Void EosHdWallets z) -> DB -> a -> z
liftNoErrorsEosHd1 f db a = liftNoErrorsEosHd0 (f a) db
