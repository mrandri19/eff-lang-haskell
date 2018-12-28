module Typesystem where

import           Syntax
import qualified Data.Map.Strict               as Map

data ValueType =
    VTBool
    | VTUnit
    | VTNum
    | VTString
    | VTFunction ValueType ComputationType
    | VTHandler ComputationType ComputationType

data ComputationType = CType ValueType [(OperationName, Type, Type)]

data Error = Mismatch | VariableNotFound
data Type =
    ValueType ValueType
    | ComputationType ComputationType
    | Void

type Env = Map.Map VariableName Type
emptyEnv :: Map.Map VariableName Type
emptyEnv = Map.empty



checkValue :: Value -> Env -> Either Error Type
checkValue (VVar x) env = case env Map.!? x of
  Just t  -> Right t
  Nothing -> Left VariableNotFound
checkValue (VBool _)    _   = return $ ValueType VTBool
checkValue VUnit        _   = return $ ValueType VTUnit
checkValue (VNum    _ ) _   = return $ ValueType VTNum
checkValue (VString _ ) _   = return $ ValueType VTString
-- TODO: I don't know what to do with this:
-- do I add the type in the syntax directly?
checkValue (VFun x c  ) env = undefined
checkValue (VHandler h) _   = undefined

-- TODO: finish
checkComputation :: Computation -> Env -> Either Error Type
checkComputation (CReturn v) env = do
  t <- checkValue v env
  case t of
    ValueType v -> return $ ComputationType (CType v [])
    _           -> undefined
checkComputation (COperation _ _ _ _) _ = undefined
checkComputation (CDo _ _ _         ) _ = undefined
checkComputation (CIf _ _ _         ) _ = undefined
checkComputation (CApp  _ _         ) _ = undefined
checkComputation (CWith _ _         ) _ = undefined


