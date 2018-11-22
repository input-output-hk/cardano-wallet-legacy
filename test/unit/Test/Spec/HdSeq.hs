module Test.Spec.HdSeq (
    spec
  ) where

import           Universum

import           Test.Hspec (Spec, describe, it, shouldBe)

{-------------------------------------------------------------------------------
  Spec
-------------------------------------------------------------------------------}

spec :: Spec
spec =  describe "AddressPool" $
            it "AccountPools" $
                True `shouldBe` True
