module Typesystem where

import qualified Data.Map.Strict               as Map

import           TypedSyntax
import           Type


data Error = Mismatch | VariableNotFound
    deriving (Show, Eq, Ord)

checkValue :: Value -> Env -> Either Error Type

-- Rule 1
checkValue (VVar x) env = case env Map.!? x of
  Just t  -> Right t
  Nothing -> Left VariableNotFound

-- Rules 2-3
checkValue (VBool _)          _   = return $ ValueType VTBool
checkValue VUnit              _   = return $ ValueType VTUnit
checkValue (VNum    _       ) _   = return $ ValueType VTNum
checkValue (VString _       ) _   = return $ ValueType VTString

-- Rule 4
checkValue (VFun x argType c) env = do
  -- Get the type of the computation c given the hypoteses:
  -- gamma, x: A (A === argType)
  computationType <- checkComputation c (extendEnv x (ValueType argType) env)
  case computationType of
    ComputationType ct -> return $ ValueType $ VTFunction argType ct
    _                  -> Left Mismatch

checkValue (VHandler h) _ = undefined

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


