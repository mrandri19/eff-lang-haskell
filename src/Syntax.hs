-- TODO: fix exports
module Syntax where


type VariableName = String
type ContinuationName = String
type OperationName = String

data Value =
    VVar VariableName -- Variable
    | VBool Bool -- Boolean
    | VUnit -- Unit
    | VNum Int -- Number
    | VString String -- String
    | VFun VariableName Computation -- Lambda
    | VHandler Handler -- Handler
    deriving (Show, Eq, Ord)

data Handler = Handler {
    returnClause :: Maybe (VariableName, Computation),
    operationClauses :: [(OperationName, VariableName, ContinuationName, Computation)]
}
    deriving (Show, Eq, Ord)

data Computation =
    CReturn Value -- Return a value
    | COperation OperationName Value VariableName Computation -- Perform an operation
    | CDo VariableName Computation Computation -- Do block
    | CIf Value Computation Computation -- If statement
    | CApp Value Value -- Function application
    | CWith Handler Computation -- Handle a computation with an handler
    deriving (Show, Eq, Ord)
