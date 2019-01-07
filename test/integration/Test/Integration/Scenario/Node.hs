{-# LANGUAGE NamedFieldPuns #-}
module Test.Integration.Scenario.Node (spec) where

import           Universum

import           Network.HTTP.Types (Status (..))
import           Servant.Client (GenResponse (..), ServantError (..))
import           Test.Hspec (Spec, expectationFailure, it, shouldSatisfy)

import           Cardano.Wallet.Client.Http (WalletHttpClient)
import qualified Cardano.Wallet.Client.Http as Client
import           Pos.Node.API (ForceNtpCheck (..))

spec :: WalletHttpClient -> Spec
spec client = do
    it "getNodeSettings" $ do
        response <- Client.getNodeSettings client
        response `shouldSatisfy` isRight
    it "getNodeInfo ForceNtpCheck" $ do
        response <- Client.getNodeInfo client ForceNtpCheck
        response `shouldSatisfy` isRight
    it "getNodeInfo NoNtpCheck" $ do
        response <- Client.getNodeInfo client NoNtpCheck
        response `shouldSatisfy` isRight
    it "nextUpdate" $ do
        eresponse <- Client.nextUpdate client
        case eresponse of
            Left (Client.ClientHttpError (FailureResponse (Response { responseStatusCode = Status { statusCode } })))
                | statusCode == 404 -> pure ()
            Right _ ->
                pure ()
            Left err ->
                expectationFailure
                    $ "Expected either a 404 response or a success response, got: "
                    <> show err

