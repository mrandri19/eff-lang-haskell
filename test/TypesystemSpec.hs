module TypesystemSpec where

import           Test.Hspec

import           TypedSyntax
import           Typesystem                     ( checkValue
                                                , Error(..)
                                                )
import           Type

spec :: Spec
spec = do
  describe "Typesystem" $ do
    it "checks the type of boolean values" $ do
      let result  = checkValue (VBool True) emptyEnv
      let correct = Right (ValueType $ VTBool)
      result `shouldBe` correct
