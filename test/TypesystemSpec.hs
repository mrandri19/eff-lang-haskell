module TypesystemSpec where

import           Test.Hspec

import           TypedSyntax
import           Typesystem                     ( checkValue )
import           Type

spec :: Spec
spec = do
  describe "Typesystem" $ do
    it "checks the type of a variable in the environment" $ do
      let result = checkValue (VVar "foo") (extendEnv "foo" (ValueType VTUnit) emptyEnv)
      let correct = Right (ValueType $ VTUnit)
      result `shouldBe` correct
    it "checks the type of boolean values" $ do
      -- true
      let result  = checkValue (VBool True) emptyEnv
      -- Bool
      let correct = Right (ValueType $ VTBool)
      result `shouldBe` correct
    it "checks the type of unit values" $ do
      -- ()
      let result  = checkValue (VUnit) emptyEnv
      -- Unit
      let correct = Right (ValueType $ VTUnit)
      result `shouldBe` correct
    it "checks the type of numeric values" $ do
      -- 42
      let result  = checkValue (VNum 42) emptyEnv
      -- Num
      let correct = Right (ValueType $ VTNum)
      result `shouldBe` correct
    it "checks the type of string values" $ do
      -- "foo"
      let result  = checkValue (VString "foo") emptyEnv
      -- String
      let correct = Right (ValueType $ VTString)
      result `shouldBe` correct
    it "checks the type of an annotated function" $ do
      -- (fun x:num -> return ())
      let result  = checkValue (VFun "x" VTNum (CReturn VUnit)) emptyEnv
      -- Num -> ()!{}
      let correct = Right (ValueType $ (VTFunction VTNum (CType VTUnit [])))
      result `shouldBe` correct
