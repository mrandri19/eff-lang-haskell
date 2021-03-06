module BigStepEval
  ( ceval
  , veval
  , emptyEnv
  , run
  , Error(..)
  )
where

import           Data.List                      ( find )
import qualified Data.Map.Strict               as Map

import           Syntax
import           Util

type Env = Map.Map VariableName Value
emptyEnv :: Map.Map VariableName Value
emptyEnv = Map.empty

-- TODO: don't return a computation, return a value instead, like a closure or a primitive val
-- Big step
ceval :: Computation -> Env -> Computation
ceval expr env = case expr of
  CReturn v                    -> CReturn (veval v env)

  -- TODO: Not sure about this veval
  CDo x (CReturn v) c          -> ceval c (Map.insert x (veval v env) env)
  -- I feel like it should be here
  CDo x (COperation op v y c1) c2 -> COperation op v y (CDo x c1 c2)
  CDo x c1 c2                  -> ceval (CDo x (ceval c1 env) c2) env

  CIf (VBool True) c1 _        -> c1
  CIf (VBool False) _ c2       -> c2
  CIf v c1 c2                  -> CIf (veval v env) c1 c2

  CApp  (VFun x c) v           -> ceval c (Map.insert x v env)
  CApp  v          v'          -> ceval (CApp (veval v env) v') env

  CWith h          (CReturn v) -> case returnClause h of
      -- TODO: Not sure about this veval
    Just (x, cr) -> ceval cr (Map.insert x (veval v env) env)
    Nothing      -> CReturn v
  CWith h (COperation op v y c) ->
      -- Find op in the list h ops
    let ops = operationClauses h
    in  case find (\(op', _, _, _) -> op' == op) ops of
          Just (_, x, k, c') ->
            let env' = Map.insert k (VFun y (CWith h c)) env
            in  let env'' = Map.insert x v env' in ceval c' env''
          Nothing -> COperation op v y (CWith h c)
  CWith h c -> CWith h (ceval c env)

  _         -> error $ "I don't know how to handle this:\n" ++ show expr ++ "\n" ++ show env

veval :: Value -> Env -> Value
veval expr env = case expr of
  VVar x -> case Map.lookup x env of
    Just v  -> v
    Nothing -> error "variable not found"
  VBool   _  -> expr
  VString _  -> expr
  VNum    _  -> expr
  VUnit      -> expr
  -- FIXME: have some way to represent a closure I suppose.
  -- This is not call by value
  -- WTF, I am evaluating a function's content before acutally executing it
  VFun x c   -> VFun x (ceval c env)
  VHandler _ -> expr

data Error = UnhandledOp OperationName deriving (Show, Eq, Ord)

top_handle :: Computation -> Env -> Either Error Value
top_handle expr env = case ceval expr env of
  CReturn v                   -> Right v

  COperation "read" VUnit y c -> do
    let new_env = Map.insert y (VString "Hello, world") env
    top_handle c new_env

  COperation op _ _ _ -> do
    Left (UnhandledOp op)

  u -> error $ show u ++ "\n" ++ show env

run :: Computation -> Either Error Value
run expr = top_handle expr emptyEnv
