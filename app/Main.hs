module Main where

import Syntax
import qualified BigStepEval as B

main :: IO ()
main = undefined

rule2test =
    B.run (CDo "x" (CReturn $ VBool True) (CReturn (VVar "x")))

rule3test =
    B.run (CDo "name" (COperation "read" VUnit "y" (CReturn $ VVar "y")) (CReturn (VVar "name")))

rule4test =
    B.run (CIf (VBool True) (CReturn $ VVar "ciao") (CReturn $ VVar "cane"))

rule5test =
    B.run (CIf (VBool False) (CReturn $ VVar "ciao") (CReturn $ VVar "cane"))

rule6test =
    B.run (CApp (VFun "x" (CReturn $ VVar "x")) (VBool True))

rule8test =
    B.run (CWith (Handler (Just("x", (CReturn $ VVar "x"))) []) (CReturn $ VBool True))

rule9test =
    B.run (CWith
        (Handler Nothing [("Print", "x", "k", CReturn $ VBool False)])
        (COperation "Print" VUnit "y" (CReturn $ VVar "y"))
    )

rule9test2 =
    B.run (CWith
        (Handler (Just ("x", (CReturn $ VVar "x"))) [("Read", "x", "k", CApp (VVar "k") (VBool True))])
        (COperation "Read" VUnit "y" (CReturn $ VVar "y"))
    )

rule10test =
    B.run (CWith
        (Handler Nothing [("Print", "x", "k", CReturn $ VBool False)])
        (COperation "Fetch" VUnit "y" (CReturn $ VVar "y"))
        )
