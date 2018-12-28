import           Test.Hspec

import           BigStepEval                    ( run
                                                , Error(..)
                                                )
import           Syntax

main :: IO ()
main = hspec $ do
  describe "Big step tree-walking evaluator" $ do
    it "evaluates do-return" $ do
      -- do x <- return true in return x
      result  <- (run (CDo "x" (CReturn $ VBool True) (CReturn (VVar "x"))))
      -- true
      correct <- (return (Right (VBool True)))
      result `shouldBe` correct

    it "evaluates do-op" $ do
      -- do name <- read () in return name
      result <- run
        (CDo "name" (COperation "read" VUnit "y" (CReturn $ VVar "y")) (CReturn (VVar "name")))
      -- name == "Hello, world" (Hardcoded in BigStepEvaluator) (TODO: make better)
      correct <- return (Right (VString "Hello, world"))
      result `shouldBe` correct

    it "evaluates if-true" $ do
      -- if true then return ciao else return cane
      result  <- run (CIf (VBool True) (CReturn $ VVar "ciao") (CReturn $ VVar "cane"))
      -- ciao
      correct <- return (Right (VVar "ciao"))
      result `shouldBe` correct

    it "evaluates if-false" $ do
      -- if false then return ciao else return cane
      result  <- run (CIf (VBool False) (CReturn $ VVar "ciao") (CReturn $ VVar "cane"))
      -- cane
      correct <- return (Right (VVar "cane"))
      result `shouldBe` correct

    it "evaluates functions" $ do
      -- (fun x -> return x) true
      result  <- run (CApp (VFun "x" (CReturn $ VVar "x")) (VBool True))
      -- true
      correct <- return (Right (VBool True))
      result `shouldBe` correct

    it "evaluates closures" $ do
      -- do b <- (do a <- return 12 in return (fun x -> return a)) in b ()
      result <- run
        (CDo "b"
             (CDo "a" (CReturn (VNum 12)) (CReturn (VFun "x" (CReturn (VVar "a")))))
             (CApp (VVar "b") VUnit)
        )
      -- 12
      correct <- return (Right (VNum 12))
      result `shouldBe` correct

    it "evaluates handler returns" $ do
      -- with {return x -> return x} handle (return true)
      result  <- run (CWith (Handler (Just ("x", (CReturn $ VVar "x"))) []) (CReturn $ VBool True))
      -- true
      correct <- return (Right (VBool True))
      result `shouldBe` correct


    it "evaluates handler op returns" $ do
      -- with {Print x k -> return false} handle (print ())
      result <- run
        (CWith (Handler Nothing [("print", "x", "k", CReturn $ VBool False)])
               (COperation "print" VUnit "y" (CReturn $ VVar "y"))
        )
      -- false
      correct <- return (Right (VBool False))
      result `shouldBe` correct

    it "evaluates handler op continuations" $ do
      -- with {return x -> return x; read x k -> k "Hello, world" } handle (read ())
      result <- run
        (CWith
          (Handler (Just ("x", (CReturn $ VVar "x")))
                   [("read", "x", "k", CApp (VVar "k") (VString "Hello, world"))]
          )
          (COperation "read" VUnit "y" (CReturn $ VVar "y"))
        )
      -- "Hello, world"
      correct <- return (Right (VString "Hello, world"))
      result `shouldBe` correct

    it "handles unexistings ops" $ do
      -- with {print k x -> return false} handle (fetch ())
      result <- run
        (CWith (Handler Nothing [("print", "x", "k", CReturn $ VBool False)])
               (COperation "fetch" VUnit "y" (CReturn $ VVar "y"))
        )
      correct <- return (Left (UnhandledOp "fetch"))
      result `shouldBe` correct
