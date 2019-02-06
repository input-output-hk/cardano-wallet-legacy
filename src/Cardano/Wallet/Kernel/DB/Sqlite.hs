-- | Sqlite database for the 'TxMeta' portion of the wallet kernel.
module Cardano.Wallet.Kernel.DB.Sqlite (

    -- * Resource creation and acquisition
      newConnection
    , closeMetaDB

    -- * Clear all entries
    , clearMetaDB

    -- * Basic API
    , putTxMeta
    , getTxMeta
    , getTxMetas
    , deleteTxMetas

    -- * Unsafe functions
    , unsafeMigrateMetaDB

    -- * testing
    , mkInputs
    , fromInputs
    , mkOutputs
    , fromOutputs
    , putTxMetaT
    , getAllTxMetas
    , getTxMetasTable
    , getInputsTable
    , getOutputsTable
    ) where

import Cardano.Wallet.Kernel.DB.Sqlite.Persistent