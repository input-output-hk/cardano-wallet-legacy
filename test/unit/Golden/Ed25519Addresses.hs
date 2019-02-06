module Golden.Ed25519Addresses
    ( spec
    ) where

import           Universum hiding (pretty)

import           Crypto.Encoding.BIP39 (ConsistentEntropy, EntropySize)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Test.Hspec (Spec, describe, it, shouldBe)

import           Cardano.Mnemonic (Mnemonic, mkMnemonic)
import           Cardano.Wallet.Kernel.Ed25519Bip44 (ChangeChain (..),
                     deriveAddressKeyPair, genEncryptedSecretKey,
                     isInternalChange)
import           Pos.Core (Address)
import           Pos.Core.Common (IsBootstrapEraAddr (..), decodeTextAddress,
                     makePubKeyAddress)
import           Pos.Core.NetworkMagic (NetworkMagic (..))


data TestVector = forall n. KnownNat n => TestVector
    { _address
        :: Address -- | Corresponding Cardano Address
    , _accountIx
        :: Word32 -- | Account index, hardened
    , _changeChain
        :: ChangeChain -- | Whether this is a change address or not
    , _addressIx
        :: Word32 -- | Address index, non-hardened
    , _mnemonic
        :: Mnemonic n -- | @n@ mnemonic words
    , _mnemonicPassphrase
        :: ByteString -- | A optional passphrase for mnemonic protection.
    }

spec :: Spec
spec = do
    describe "Ed25519 Addresses - Golden Tests" $ forM_ tests
        $ \vec@(TestVector addr accIx change addrIx mw pw) -> it (titleize vec) $ do
            let rootPassphrase = mempty
            let esk = genEncryptedSecretKey (mw, pw) rootPassphrase
            let pubKey = fst <$> deriveAddressKeyPair rootPassphrase esk accIx change addrIx
            let addr' = makePubKeyAddress NetworkMainOrStage (IsBootstrapEraAddr True) <$> pubKey
            addr' `shouldBe` (Just addr)
  where
    tests :: [TestVector]
    tests =
        [ TestVector
            { _address = unsafeMkAddress "Ae2tdPwUPEZ1DYmhvpJWtVkMUbypPVkCVjQLNJeKRRG4LJ64dqTSRpWqzLH"
            , _accountIx = 0x80000000
            , _changeChain = ExternalChain
            , _addressIx = 0x00000000
            , _mnemonic = unsafeMkMnemonic @24
                [ "process", "sauce", "ahead"
                , "chase", "away", "ability"
                , "odor", "excuse", "immune"
                , "local", "climb", "wrestle"
                , "vanish", "annual", "mimic"
                , "square", "light", "corn"
                , "dance", "fun", "april"
                , "rail", "view", "certain"
                ]
            , _mnemonicPassphrase = "Cardano the cardano that cardano!"
            }

        , TestVector
            { _address = unsafeMkAddress "Ae2tdPwUPEZ7ZyqyuDKkCnjrRjTY1vMJ8353gD7XWrUYufpVwEPhKwseVvf"
            , _accountIx = 0x80000000
            , _changeChain = ExternalChain
            , _addressIx = 0x0000000E
            , _mnemonic = unsafeMkMnemonic @24
                [ "process", "sauce", "ahead"
                , "chase", "away", "ability"
                , "odor", "excuse", "immune"
                , "local", "climb", "wrestle"
                , "vanish", "annual", "mimic"
                , "square", "light", "corn"
                , "dance", "fun", "april"
                , "rail", "view", "certain"
                ]
            , _mnemonicPassphrase = "Cardano the cardano that cardano!"
            }

        , TestVector
            { _address = unsafeMkAddress "Ae2tdPwUPEZLSqQN7XNJRMJ6yHWdfFLaQgPPYgyJKrJnCVnRtbfw6EHRv1D"
            , _accountIx = 0x8000000E
            , _changeChain = InternalChain
            , _addressIx = 0x0000002A
            , _mnemonic = unsafeMkMnemonic @24
                [ "process", "sauce", "ahead"
                , "chase", "away", "ability"
                , "odor", "excuse", "immune"
                , "local", "climb", "wrestle"
                , "vanish", "annual", "mimic"
                , "square", "light", "corn"
                , "dance", "fun", "april"
                , "rail", "view", "certain"
                ]
            , _mnemonicPassphrase = "Cardano the cardano that cardano!"
            }

        , TestVector
            { _address = unsafeMkAddress "Ae2tdPwUPEZGQVrA6qKreDzdtYxcWMMrpTFYCpFcuJfhJBEfoeiuW4MtaXZ"
            , _accountIx = 0x80000000
            , _changeChain = ExternalChain
            , _addressIx = 0x00000000
            , _mnemonic = unsafeMkMnemonic @15
                [ "cruise", "legend", "nasty"
                , "impose", "desk", "motion"
                , "pistol", "rely", "camp"
                , "journey", "drill", "spirit"
                , "among", "basic", "frequent"
                ]
            , _mnemonicPassphrase = mempty
            }

        , TestVector
            { _address = unsafeMkAddress "Ae2tdPwUPEZDLWQQEBR1UW7HeXJVaqUnuw8DUFu52TDWCJbxbkCyQYyxckP"
            , _accountIx = 0x80000000
            , _changeChain = ExternalChain
            , _addressIx = 0x0000000E
            , _mnemonic = unsafeMkMnemonic @15
                [ "cruise", "legend", "nasty"
                , "impose", "desk", "motion"
                , "pistol", "rely", "camp"
                , "journey", "drill", "spirit"
                , "among", "basic", "frequent"
                ]
            , _mnemonicPassphrase = mempty
            }

        , TestVector
            { _address = unsafeMkAddress "Ae2tdPwUPEZFRbyhz3cpfC2CumGzNkFBN2L42rcUc2yjQpEkxDbkPodpMAi"
            , _accountIx = 0x8000000E
            , _changeChain = InternalChain
            , _addressIx = 0x0000002A
            , _mnemonic = unsafeMkMnemonic @15
                [ "cruise", "legend", "nasty"
                , "impose", "desk", "motion"
                , "pistol", "rely", "camp"
                , "journey", "drill", "spirit"
                , "among", "basic", "frequent"
                ]
            , _mnemonicPassphrase = mempty
            }
        ]


--
-- Internals
--

class UnsafeMkMnemonic mw where
    unsafeMkMnemonic
        :: (ConsistentEntropy n mw csz, EntropySize mw ~ n)
        => [Text]
        -> Mnemonic mw
    unsafeMkMnemonic ws =
        case mkMnemonic ws of
            Left e ->
                error $
                    "Golden.Ed25519Addresses: failed to create mnemonic for the \
                    \test vector using the given words: " <> show ws
                    <> "\n" <> show e
            Right m -> m
instance UnsafeMkMnemonic 24
instance UnsafeMkMnemonic 15

unsafeMkAddress
    :: Text
    -> Address
unsafeMkAddress txt =
    case decodeTextAddress txt of
        Left e ->
            error $
                "Golden.Ed25519Addresses: failed to create address for the \
                \test vector using the given base58 text: " <> show txt
                <> "\n" <> show e
        Right a -> a

titleize
    :: TestVector
    -> String
titleize (TestVector _ accIx change addrIx (mw :: Mnemonic n) pw) =
    mwS <> " (n = " <> nS <> ") " <> pwS <> " " <> accIxS <> "/" <> changeS <> "/" <> addrIxS
  where
    ellipse n bs = BS.take n bs <> "..." <> BS.drop (BS.length bs - n) bs
    nS = show $ natVal $ Proxy @n
    mwS = T.unpack $ T.decodeUtf8 $ ellipse 26 (BL.toStrict $ Aeson.encode mw)
    pwS = if null pw then "no passphrase  " else "with passphrase"
    accIxS = show (accIx - 0x80000000) <> "'"
    changeS = if isInternalChange change then "1" else "0"
    addrIxS = show addrIx
