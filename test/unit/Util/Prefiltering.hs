module Util.Prefiltering
    ( prefilterUtxo
    ) where

import           Universum

import qualified Data.Map.Strict as Map

import           Cardano.Wallet.Kernel.DB.HdRootId (HdRootId)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId, HdAddress,
                     hdAddressId, hdAddressIdParent, isOurs)
import           Pos.Chain.Txp (TxOut (..), TxOutAux (..), Utxo)
import           Pos.Crypto (EncryptedSecretKey)

prefilterUtxo
    :: HdRootId
    -> EncryptedSecretKey
    -> Utxo
    -> Map HdAccountId (Utxo, [HdAddress])
prefilterUtxo rootId esk utxo = flip evalState [(rootId, esk)] $
    fmap (Map.unionsWith (<>)) $ forM (Map.toList utxo) $ \(txin, txout) -> do
        let addr = txOutAddress $ toaOut txout
        state (isOurs addr) <&> \case
            Nothing ->
                Map.empty
            Just hdAddr ->
                let
                    utxo' = Map.singleton txin txout
                    accId = hdAddr ^. hdAddressId . hdAddressIdParent
                in
                    Map.singleton accId (utxo', [hdAddr])
