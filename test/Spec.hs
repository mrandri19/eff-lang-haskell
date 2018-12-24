import           Control.Exception              ( evaluate )
import           Test.Hspec
import           Test.QuickCheck

import qualified BigStepEval                   as B
import           Syntax

main :: IO ()
main = hspec $ do
  describe "Big step tree-walking evaluator" $ do
    it "evaluates do-return" $ do
      -- do x <- return true in return x
      result  <- (B.run (CDo "x" (CReturn $ VBool True) (CReturn (VVar "x"))))
      -- true
      correct <- (return (Right (VBool True)))
      result `shouldBe` correct

    it "evaluates do-op" $ do
      -- do name <- read () in return name
      result <- B.run
        (CDo "name" (COperation "read" VUnit "y" (CReturn $ VVar "y")) (CReturn (VVar "name")))
      -- name == "Hello, world" (Hardcoded in BigStepEvaluator) (TODO: make better)
      correct <- return (Right (VString "Hello, world"))
      result `shouldBe` correct

    it "evaluates if-true" $ do
      -- if true then return ciao else return cane
      result  <- B.run (CIf (VBool True) (CReturn $ VVar "ciao") (CReturn $ VVar "cane"))
      -- ciao
      correct <- return (Right (VVar "ciao"))
      result `shouldBe` correct

    it "evaluates if-false" $ do
      -- if false then return ciao else return cane
      result  <- B.run (CIf (VBool False) (CReturn $ VVar "ciao") (CReturn $ VVar "cane"))
      -- cane
      correct <- return (Right (VVar "cane"))
      result `shouldBe` correct

    it "evaluates functions" $ do
      -- (fun x -> return x) true
      result  <- B.run (CApp (VFun "x" (CReturn $ VVar "x")) (VBool True))
      -- true
      correct <- return (Right (VBool True))
      result `shouldBe` correct

    it "evaluates handler returns" $ do
      -- with {return x -> return x} handle (return true)
      result <- B.run (CWith (Handler (Just ("x", (CReturn $ VVar "x"))) []) (CReturn $ VBool True))
      -- true
      correct <- return (Right (VBool True))
      result `shouldBe` correct


    it "evaluates handler op returns" $ do
      -- with {Print x k -> return false} handle (print ())
      result <- B.run
        (CWith (Handler Nothing [("print", "x", "k", CReturn $ VBool False)])
               (COperation "print" VUnit "y" (CReturn $ VVar "y"))
        )
      -- false
      correct <- return (Right (VBool False))
      result `shouldBe` correct

    it "evaluates handler op continuations" $ do
      -- with {return x -> return x; read x k -> k "Hello, world" } handle (read ())
      result <- B.run
        (CWith
          (Handler (Just ("x", (CReturn $ VVar "x")))
                   [("read", "x", "k", CApp (VVar "k") (VString "Hello, world"))]
          )
          (COperation "read" VUnit "y" (CReturn $ VVar "y"))
        )
      -- "Hello, world"
      correct <- return (Right (VString "Hello, world"))
      result `shouldBe` correct

    it "evaluates unexistings ops" $ do
      -- with {print k x -> return false} handle (fetch ())
      result <- B.run
        (CWith (Handler Nothing [("Print", "x", "k", CReturn $ VBool False)])
               (COperation "fetch" VUnit "y" (CReturn $ VVar "y"))
        )
      correct <- return (Left B.UnhandledOp)
      result `shouldBe` correct
  describe "Example programs" $ do
    -- TODO: finish


