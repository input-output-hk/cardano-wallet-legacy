-- | UPDATE operations on HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet.Update (
    updateHdRoot
  , updateHdRootPassword
  , updateHdRootGap
  , updateHdAccountName
  ) where

import           Universum

import           Formatting (build, sformat)

import           Cardano.Wallet.Kernel.AddressPoolGap (AddressPoolGap)
import           Cardano.Wallet.Kernel.DB.HdRootId (HdRootId)
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           UTxO.Util (modifyAndGetNew)

{-------------------------------------------------------------------------------
  UPDATE
-------------------------------------------------------------------------------}

-- | Updates in one gulp the Hd Wallet name and assurance level.
updateHdRoot :: HdRootId
             -> AssuranceLevel
             -> WalletName
             -> Update' UnknownHdRoot HdWallets HdRoot
updateHdRoot rootId assurance name =
    zoomHdRootId identity rootId $ do
        modifyAndGetNew $ set hdRootAssurance assurance . set hdRootName name

updateHdRootPassword :: HdRootId
                     -> HasSpendingPassword
                     -> Update' UnknownHdRoot HdWallets HdRoot
updateHdRootPassword rootId hasSpendingPassword =
    zoomHdRootId identity rootId $ do
        modifyAndGetNew $ hdRootHasPassword .~ hasSpendingPassword

updateHdRootGap
    :: HdRootId
    -> AddressPoolGap
    -> Update' UnknownHdRoot HdWallets HdRoot
updateHdRootGap rootId gap =
    zoomHdRootId identity rootId $ do
        root <- get
        case root ^. hdRootBase of
            HdRootExternallyOwned _ _ accounts ->
                modifyAndGetNew $ hdRootBase .~ HdRootExternallyOwned rootId gap accounts
            HdRootFullyOwned _ ->
                error $
                    "Cardano.Wallet.Kernel.DB.HdWallet.Update: implementation error! \
                    \We've been trying to update an AddressPoolGap of a an HdRoot \
                    \that isn't externally-owned (" <> sformat build rootId <> "). \
                    \This should never happen and our DB-layer should protect us \
                    \from such scenario. This means something is seriously wrong \
                    \in our implementation and we are treating fully owned wallet \
                    \as-if they were externally owned somewhere else in the calling code!"

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update' UnknownHdAccount HdWallets HdAccount
updateHdAccountName accId name = do
    zoomHdAccountId identity accId $ do
        modifyAndGetNew $ hdAccountName .~ name
