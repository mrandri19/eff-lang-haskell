-- TODO: fix exports
module Lib where

import Data.List (find)

type VariableName = String
type ContinuationName = String
type OperationName = String

data Value = VVar VariableName
        | VBool Bool
        | VUnit
        | VFun VariableName Computation
        | VHandler Handler
    deriving (Show, Eq, Ord)

data Handler = Handler {
    returnClause :: Maybe (VariableName, Computation),
    operationClauses :: [(OperationName, VariableName, ContinuationName, Computation)]
}
    deriving (Show, Eq, Ord)


data Computation = CReturn Value
        | COperation OperationName Value VariableName Computation
        | CDo VariableName Computation Computation
        | CIf Value Computation Computation
        | CApp Value Value
        | CWith Handler Computation
    deriving (Show, Eq, Ord)

substValueIntoComputation :: Value -> VariableName -> Computation -> Computation
substValueIntoComputation v x c = case c of
    CReturn v' -> CReturn (substValueIntoValue v x v')
    CApp v1 v2 -> CApp (substValueIntoValue v x v1) (substValueIntoValue v x v2)

substValueIntoValue :: Value -> VariableName -> Value -> Value
substValueIntoValue v x v' = case v' of
    VVar x' | x' == x -> v
    VVar x' | x' /= x -> v'
    VBool _ -> v'
    VUnit -> v'

-- Small step
eval :: Computation -> Computation
eval expr = case expr of
    -- Rule 2
    CDo x (CReturn v) c -> substValueIntoComputation v x c

    -- Rule 3
    CDo x (COperation op v y c1) c2 ->
        COperation op v y (CDo x c1 c2)

    -- Rule 1
    CDo x c1 c2 -> CDo x (eval c1) c2

    -- Rule 4
    CIf (VBool True) c1 c2 -> c1

    -- Rule 5
    CIf (VBool False) c1 c2 -> c2

    -- Rule 6
    CApp (VFun x c) v -> substValueIntoComputation v x c

    -- Rule 8
    CWith h (CReturn v) -> case returnClause h of
        Just (x,cr) -> substValueIntoComputation v x cr
        Nothing -> error "Cannot handle a return without a return clause in the handler"

    -- Rule 9,10
    CWith h (COperation op v y c) ->
        -- Find op in the list h ops
        let ops = operationClauses h in
        case find (\(op',_,_,_) -> op' == op) ops of

            -- Rule 9
            Just (_,x,k,c') ->
                substValueIntoComputation
                    (VFun y (
                        CWith h c
                    ))
                    k
                    (substValueIntoComputation v x c')

            -- Rule 10
            Nothing ->
                COperation op v y (CWith h c)

    -- Rule 7
    CWith h c -> CWith h (eval c)

    _ -> undefined


rule2test =
    eval (CDo "x" (CReturn $ VBool True) (CReturn (VVar "x")))

rule3test =
    eval (CDo "name" (COperation "read" VUnit "y" (CReturn $ VVar "y")) (CReturn (VVar "name")))

rule4test =
    eval (CIf (VBool True) (CReturn $ VVar "ciao") (CReturn $ VVar "cane"))

rule5test =
    eval (CIf (VBool False) (CReturn $ VVar "ciao") (CReturn $ VVar "cane"))

rule6test =
    eval (CApp (VFun "x" (CReturn $ VVar "x")) (VBool True))

rule8test =
    eval (CWith (Handler (Just("x", (CReturn $ VVar "x"))) []) (CReturn $ VBool True))

rule9test =
    eval (CWith
        (Handler Nothing [("Print", "x", "k", CReturn $ VBool False)])
        (COperation "Print" VUnit "y" (CReturn $ VVar "y"))
    )

rule9test2 =
    eval (CWith
        (Handler Nothing [("Read", "x", "k", CApp (VVar "k") (VBool True))])
        (COperation "Read" VUnit "y" (CReturn $ VVar "y"))
    )

rule10test =
    eval (CWith
        (Handler Nothing [("Print", "x", "k", CReturn $ VBool False)])
        (COperation "Fetch" VUnit "y" (CReturn $ VVar "y"))
        )
