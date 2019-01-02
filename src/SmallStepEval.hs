module SmallStepEval where

import           Data.List                      ( find )
import qualified Data.Set                      as Set

import           Syntax
import           Util

-- Small step
eval :: Computation -> Computation
eval expr = case expr of
    -- Rule 2
  CDo x             (CReturn v           ) c  -> substValueIntoComputation v x c

  -- Rule 3
  CDo x             (COperation op v y c1) c2 -> COperation op v y (CDo x c1 c2)

  -- Rule 1
  CDo x             c1                     c2 -> CDo x (eval c1) c2

  -- Rule 4
  CIf (VBool True ) c1                     _  -> c1

  -- Rule 5
  CIf (VBool False) _                      c2 -> c2

  -- Rule 6
  CApp  (VFun x c) v                          -> substValueIntoComputation v x c

  -- Rule 8
  CWith h          (CReturn v)                -> case returnClause h of
    Just (x, cr) -> substValueIntoComputation v x cr
    Nothing      -> error "Cannot handle a return without a return clause in the handler"

  -- Rule 9,10
  CWith h (COperation op v y c) ->
      -- Find op in the list h ops
    let ops = operationClauses h
    in  case find (\(op', _, _, _) -> op' == op) ops of

                                                                    -- Rule 9
          Just (_, x, k, c') ->
            substValueIntoComputation (VFun y (CWith h c)) k (substValueIntoComputation v x c')

          -- Rule 10
          Nothing -> COperation op v y (CWith h c)

  -- Rule 7
  CWith h c -> CWith h (eval c)

  _         -> undefined

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
