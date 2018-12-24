-- TODO: fix exports
module Syntax where

import qualified Data.Set                      as Set

type VariableName = String
type ContinuationName = String
type OperationName = String

data Value = VVar VariableName
        | VBool Bool
        | VUnit
        | VString String
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

freeVariablesOfValue :: Value -> Set.Set VariableName
freeVariablesOfValue v = case v of
    VVar    name -> Set.singleton name
    VBool   _    -> Set.empty
    VString _    -> Set.empty
    VUnit        -> Set.empty
    VFun x c     -> x `Set.delete` (freeVariablesOfComputation c)
    VHandler _   -> undefined

freeVariablesOfComputation :: Computation -> Set.Set VariableName
freeVariablesOfComputation c = case c of
    CReturn v -> freeVariablesOfValue v

substValueIntoValue :: Value -> VariableName -> Value -> Value
substValueIntoValue v x v' = case v' of
    VUnit             -> v'
    VString _         -> v'
    VBool   _         -> v'
    VVar x' | x' == x -> v
    VVar x' | x' /= x -> v'
    VFun x' c | ((x' /= x) && (x' `notElem` (freeVariablesOfValue v))) ->
        VFun x' (substValueIntoComputation v x c)

substValueIntoComputation :: Value -> VariableName -> Computation -> Computation
substValueIntoComputation v x c = case c of
    CReturn v' -> CReturn (substValueIntoValue v x v')
    CApp v1 v2 -> CApp (substValueIntoValue v x v1) (substValueIntoValue v x v2)
