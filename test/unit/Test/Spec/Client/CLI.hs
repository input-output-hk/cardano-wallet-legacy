module Test.Spec.Client.CLI (spec) where

import           Data.Either
import           Test.Hspec
import           Universum

import           Pos.Core (Coin (..))

import           Cardano.Wallet.Client.CLI.Options (coinAmountParser)

spec :: Spec
spec = do
    describe "The amount option parser" $ do
        it "understands a number to be lovelace" $
            coinAmountParser "12345" `shouldBe` Right (Coin 12345)

        it "understands lovelace suffix" $
            coinAmountParser "12345 lovelace" `shouldBe` Right (Coin 12345)

        it "understands ada suffix" $
            coinAmountParser "12345 ada" `shouldBe` Right (Coin 12345000000)

        it "won't return an amount greater than the max" $
            coinAmountParser (replicate 15 '9') ++ "ada") `shouldSatisfy` isLeft

        it "consumes all input" $
            coinAmountParser "56787xx" `shouldSatisfy` isLeft

        it "needs a value" $
            coinAmountParser "qwerty" `shouldSatisfy` isLeft

        it "is case-insensitive" $
            coinAmountParser "123 ADA" `shouldBe` Right (Coin 123000000)

        it "ignores whitespace before unit" $
            coinAmountParser "12345Ada" `shouldBe` Right (Coin 12345000000)

        xit "understands decimal ada values 1" $
            coinAmountParser "0.123456 ada" `shouldBe` Right (Coin 123456)

        xit "understands decimal ada values 2" $
            coinAmountParser "123.456789 ada" `shouldBe` Right (Coin 123456789)

        xit "understands decimal ada values -- missing leading 0" $
            coinAmountParser ".123456 ada" `shouldBe` Right (Coin 123456)

        xit "understands decimal ada values -- missing digits" $
            coinAmountParser ".123 ada" `shouldBe` Right (Coin 123000)
