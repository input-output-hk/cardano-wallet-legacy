{-# LANGUAGE TypeApplications #-}
module Test.Spec.NewPayment (
      spec

    -- Public to be used by other testing modules.
    , Fixture (..)
    , withFixture
    , withPayment
  ) where

import           Universum

import           Control.Lens (to)

import           Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, choose, withMaxSuccess)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)

import qualified Data.Map.Strict as M

import           Data.Acid (update)
import           Formatting (build, formatToString, sformat)

import           Pos.Chain.Txp (TxOut (..), TxOutAux (..))
import           Pos.Core (Address, Coin (..), IsBootstrapEraAddr (..),
                     deriveLvl2KeyPair, getCurrentTimestamp,
                     makePubKeyAddressBoot, mkCoin)
import           Pos.Core.NetworkMagic (NetworkMagic (..), makeNetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, ProtocolMagic,
                     ShouldCheckPassphrase (..), safeDeterministicKeyGen)

import           Test.Spec.CoinSelection.Generators (InitialBalance (..),
                     Pay (..), genPayeeWithNM, genUtxoWithAtLeast)

import           Cardano.Mnemonic (Mnemonic, mnemonicToSeed)
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.Ed25519Bip44
                     (ChangeChain (ExternalChain), deriveAddressKeyPair,
                     genEncryptedSecretKey)

import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (CoinSelectionOptions (..), ExpenseRegulation (..),
                     InputGrouping (..), newOptions)
import           Cardano.Wallet.Kernel.DB.AcidState
import           Cardano.Wallet.Kernel.DB.HdRootId (HdRootId, eskToHdRootId)
import           Cardano.Wallet.Kernel.DB.HdWallet (AssuranceLevel (..),
                     HasSpendingPassword (..), HdAccountId (..),
                     HdAccountIx (..), HdAddressIx (..), WalletName (..),
                     hdAccountIdIx)
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (initHdRoot)
import           Cardano.Wallet.Kernel.DB.HdWallet.Derivation
                     (HardeningMode (..), deriveIndex)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.Internal (ActiveWallet, PassiveWallet,
                     wallets)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import qualified Cardano.Wallet.Kernel.Transactions as Kernel
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer
import qualified Cardano.Wallet.WalletLayer.Kernel.Conv as Kernel.Conv
import qualified Util.Prefiltering as Kernel

import qualified Test.Spec.Fixture as Fixture
import           Util.Buildable (ShowThroughBuild (..))

import qualified Cardano.Wallet.API.V1.Handlers.Transactions as Handlers
import           Control.Monad.Except (runExceptT)
import           Servant.Server


{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data Fixture = Fixture {
      fixtureHdRootId   :: HdRootId
    , fixtureESK        :: EncryptedSecretKey
    , fixtureAccountId  :: HdAccountId
    , fixturePw         :: PassiveWallet
    , fixturePayees     :: NonEmpty (Address, Coin)
    , fixturePassPhrase :: PassPhrase
    }

-- | Prepare some fixtures using the 'PropertyM' context to prepare the data,
-- and execute the 'acid-state' update once the 'PassiveWallet' gets into
-- scope (after the bracket initialisation).
prepareFixtures :: NetworkMagic
                -> InitialBalance
                -> Pay
                -> Fixture.GenActiveWalletFixture Fixture
prepareFixtures nm initialBalance toPay = do
    addressVersion <- return Kernel.AddressSchemeSeq -- pick arbitrary
    passPhrase <- pick $ arbitrary @PassPhrase
    mnemonic <- pick $ arbitrary @(Mnemonic 12)
    let seed = mnemonicToSeed mnemonic
    let esk =
            case addressVersion of
                Kernel.AddressSchemeRnd -> snd $ safeDeterministicKeyGen seed passPhrase
                Kernel.AddressSchemeSeq -> genEncryptedSecretKey (mnemonic, mempty) passPhrase
    let newRootId = eskToHdRootId nm esk
    now <- getCurrentTimestamp
    newRoot <- initHdRoot <$> pure newRootId
                          <*> pure (WalletName "A wallet")
                          <*> pure (HasSpendingPassword $ InDb now)
                          <*> pure AssuranceLevelNormal
                          <*> (InDb <$> pick arbitrary)
    utxo   <- pick (genUtxoWithAtLeast initialBalance)
    newAccountId <- case addressVersion of
        Kernel.AddressSchemeRnd -> HdAccountId newRootId <$> deriveIndex (pick . choose) HdAccountIx HardDerivation
        Kernel.AddressSchemeSeq -> pure $ HdAccountId newRootId (HdAccountIx 0x80000000) -- 2^31
    -- Override all the addresses of the random Utxo with something meaningful,
    -- i.e. with 'Address'(es) generated in a principled way, and not random.
    utxo' <- foldlM (\acc ((txIn, (TxOutAux (TxOut _ coin))), seqIndex) -> do
                        -- TODO: why is hardened derivation in rnd address derivation step? Shouldn't it be soft derivation?
                        rndIndex <- deriveIndex (pick . choose) HdAddressIx HardDerivation

                        let Just (addr, _) =
                                case addressVersion of
                                    Kernel.AddressSchemeRnd -> deriveLvl2KeyPair nm
                                                            (IsBootstrapEraAddr True)
                                                            (ShouldCheckPassphrase True)
                                                            passPhrase
                                                            esk
                                                            (newAccountId ^. hdAccountIdIx . to getHdAccountIx)
                                                            (getHdAddressIx rndIndex)
                                    Kernel.AddressSchemeSeq -> first (makePubKeyAddressBoot nm) <$> deriveAddressKeyPair
                                                            passPhrase
                                                            esk
                                                            (newAccountId ^. hdAccountIdIx . to getHdAccountIx)
                                                            ExternalChain
                                                            seqIndex
                        return $ M.insert txIn (TxOutAux (TxOut addr coin)) acc
                    ) M.empty (zip (M.toList utxo) [0..])
    payees <- fmap (\(TxOut addr coin) -> (addr, coin)) <$> pick (genPayeeWithNM nm utxo' toPay)

    return $ \keystore aw -> do
        liftIO $ Keystore.insert newRootId esk keystore
        let pw = Kernel.walletPassive aw

        let accounts    = Kernel.prefilterUtxo newRootId esk utxo'
            hdAccountId = newAccountId -- Kernel.defaultHdAccountId newRootId
            hdAddress   = Nothing -- Kernel.defaultHdAddress nm esk passPhrase newRootId

        void $ liftIO $ update (pw ^. wallets) (CreateHdWallet newRoot hdAccountId hdAddress accounts)
        return $ Fixture {
                           fixtureHdRootId = newRootId
                         , fixtureAccountId = newAccountId
                         , fixtureESK = esk
                         , fixturePw  = pw
                         , fixturePayees = payees
                         , fixturePassPhrase = passPhrase
                         }

withFixture :: MonadIO m
            => ProtocolMagic
            -> InitialBalance
            -> Pay
            -> (  Keystore.Keystore
               -> ActiveWalletLayer m
               -> ActiveWallet
               -> Fixture
               -> IO a
               )
            -> PropertyM IO a
withFixture pm initialBalance toPay cc =
    Fixture.withActiveWalletFixture pm (prepareFixtures nm initialBalance toPay) cc
  where
    nm = makeNetworkMagic pm

-- | A constant fee calculation.
constantFee :: Int -> NonEmpty Coin -> Coin
constantFee _ _ = mkCoin 10

-- | Helper function to facilitate payments via the Layer or Servant.
withPayment :: MonadIO n
            => ProtocolMagic
            -> InitialBalance
            -- ^ How big the wallet Utxo must be
            -> Pay
            -- ^ How big the payment must be
            -> (ActiveWalletLayer n -> V1.Payment -> IO ())
            -- ^ The action to run.
            -> PropertyM IO ()
withPayment pm initialBalance toPay action = do
    withFixture pm initialBalance toPay $ \keystore activeLayer _ Fixture{..} -> do
        liftIO $ Keystore.insert fixtureHdRootId fixtureESK keystore
        let sourceWallet = V1.WalletId (sformat build fixtureHdRootId)
        let accountIndex = Kernel.Conv.toAccountId fixtureAccountId
        let destinations =
                fmap (\(addr, coin) -> V1.PaymentDistribution (V1.WalAddress addr) (V1.WalletCoin coin)
                     ) fixturePayees
        let newPayment = V1.Payment {
                         pmtSource           = V1.PaymentSource sourceWallet accountIndex
                       , pmtDestinations     = destinations
                       , pmtGroupingPolicy   = Nothing
                       , pmtSpendingPassword = Just (V1.WalletPassPhrase fixturePassPhrase)
                       }
        action activeLayer newPayment

spec :: Spec
spec = describe "NewPayment" $ do

    describe "Generating a new payment (wallet layer)" $ do

        prop "pay works (realSigner, SenderPaysFee)" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withPayment pm (InitialADA 10000) (PayLovelace 10) $ \activeLayer newPayment -> do
                    res <- liftIO ((WalletLayer.pay activeLayer) IgnoreGrouping
                                                                 SenderPaysFee
                                                                 newPayment
                                  )
                    liftIO ((bimap STB STB res) `shouldSatisfy` isRight)

    describe "Generating a new payment (kernel)" $ do
        prop "newTransaction works (real signer, SenderPaysFee)" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture @IO pm (InitialADA 10000) (PayLovelace 10) $ \_ _ aw Fixture{..} -> do
                    policy <- Node.getFeePolicy (Kernel.walletPassive aw ^. Kernel.walletNode)
                    let opts = (newOptions (Kernel.cardanoFee policy)) {
                               csoExpenseRegulation = SenderPaysFee
                             , csoInputGrouping     = IgnoreGrouping
                             }
                    res <- liftIO (Kernel.newTransaction aw
                                                         fixturePassPhrase
                                                         opts
                                                         fixtureAccountId
                                                         fixturePayees
                                  )
                    liftIO ((bimap STB (const $ STB ()) res) `shouldSatisfy` isRight)
        prop "newTransaction works (ReceiverPaysFee)" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture @IO pm (InitialADA 10000) (PayADA 1) $ \_ _ aw Fixture{..} -> do
                    policy <- Node.getFeePolicy (Kernel.walletPassive aw ^. Kernel.walletNode)
                    let opts = (newOptions (Kernel.cardanoFee policy)) {
                               csoExpenseRegulation = ReceiverPaysFee
                             , csoInputGrouping     = IgnoreGrouping
                             }
                    res <- liftIO (Kernel.newTransaction aw
                                                         fixturePassPhrase
                                                         opts
                                                         fixtureAccountId
                                                         fixturePayees
                                  )
                    liftIO ((bimap STB (const $ STB ()) res) `shouldSatisfy` isRight)

    describe "Generating a new payment (Servant)" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 5 $
            monadicIO $ do
                pm <- pick arbitrary
                withPayment pm (InitialADA 1000) (PayADA 1) $ \activeLayer newPayment -> do
                    res <- liftIO (runExceptT . runHandler' $ Handlers.newTransaction activeLayer newPayment)
                    liftIO ((bimap identity STB res) `shouldSatisfy` isRight)

    describe "EstimateFees" $ do

        describe "Estimating fees (wallet layer)" $ do

            prop "estimating fees works (SenderPaysFee)" $ withMaxSuccess 5 $ do
                monadicIO $ do
                    pm <- pick arbitrary
                    withPayment pm (InitialADA 10000) (PayLovelace 10) $ \activeLayer newPayment -> do
                        res <- liftIO ((WalletLayer.estimateFees activeLayer) IgnoreGrouping
                                                                              SenderPaysFee
                                                                              newPayment
                                      )
                        case res of
                             Left e  -> fail (formatToString build e)
                             Right fee ->
                                 fee `shouldSatisfy` (> (Coin 0))

        describe "Estimating fees (kernel)" $ do
            prop "estimating fees works (SenderPaysFee)" $ withMaxSuccess 5 $
                monadicIO $ do
                    pm <- pick arbitrary
                    withFixture @IO pm (InitialADA 10000) (PayADA 1) $ \_ _ aw Fixture{..} -> do
                        let opts = (newOptions constantFee) {
                                   csoExpenseRegulation = SenderPaysFee
                                 , csoInputGrouping     = IgnoreGrouping
                                 }

                        res <- liftIO (Kernel.estimateFees aw
                                                           opts
                                                           fixtureAccountId
                                                           fixturePayees
                                      )

                        case res of
                             Left e  -> fail (formatToString build e)
                             Right x -> x `shouldBe` Coin 10

            prop "estimating fees works (kernel, ReceiverPaysFee)" $ withMaxSuccess 5 $
                monadicIO $ do
                    pm <- pick arbitrary
                    withFixture @IO pm (InitialADA 10000) (PayADA 1) $ \_ _ aw Fixture{..} -> do
                        let opts = (newOptions constantFee) {
                                   csoExpenseRegulation = SenderPaysFee
                                 , csoInputGrouping     = IgnoreGrouping
                                 }
                        res <- liftIO (Kernel.estimateFees aw
                                                           opts
                                                           fixtureAccountId
                                                           fixturePayees
                                      )

                        case res of
                             Left e  -> fail (formatToString build e)
                             Right x -> x `shouldBe` Coin 10

            prop "estimating fees works (kernel, SenderPaysFee, cardanoFee)" $ withMaxSuccess 5 $
                monadicIO $ do
                    pm <- pick arbitrary
                    withFixture @IO pm (InitialADA 10000) (PayADA 1) $ \_ _ aw Fixture{..} -> do
                        policy <- Node.getFeePolicy (Kernel.walletPassive aw ^. Kernel.walletNode)
                        let opts = (newOptions (Kernel.cardanoFee policy)) {
                                   csoExpenseRegulation = SenderPaysFee
                                 , csoInputGrouping     = IgnoreGrouping
                                 }
                        res <- liftIO (Kernel.estimateFees aw
                                                           opts
                                                           fixtureAccountId
                                                           fixturePayees
                                      )

                        case res of
                             Left e  -> fail (formatToString build e)
                             Right x -> x `shouldSatisfy` (> (Coin 0))

        describe "Estimating fees (Servant)" $ do
            prop "works as expected in the happy path scenario" $ withMaxSuccess 5 $
                monadicIO $ do
                    pm <- pick arbitrary
                    withPayment pm (InitialADA 1000) (PayADA 1) $ \activeLayer newPayment -> do
                        res <- liftIO (runExceptT . runHandler' $ Handlers.estimateFees activeLayer newPayment)
                        liftIO ((bimap identity STB res) `shouldSatisfy` isRight)

            -- The 'estimateFee' endpoint doesn't require a spendingPassword, but
            -- the 'Payment' type does come with an optional 'spendingPassword' field.
            -- Here we want to check this is completely ignored.
            prop "ignores completely the spending password in Payment" $ withMaxSuccess 5 $
                monadicIO $ do
                    randomPass <- pick arbitrary
                    pm         <- pick arbitrary
                    withPayment pm (InitialADA 1000) (PayADA 1) $ \activeLayer newPayment -> do
                        -- mangle the spending password to be something arbitrary, check
                        -- that this doesn't hinder our ability to estimate fees.
                        let pmt = newPayment { V1.pmtSpendingPassword = randomPass }
                        res <- liftIO (runExceptT . runHandler' $ Handlers.estimateFees activeLayer pmt)
                        liftIO ((bimap identity STB res) `shouldSatisfy` isRight)
