module Type where

import           Util
import qualified Data.Map.Strict               as Map

data ValueType =
    VTBool -- Boolean type
    | VTUnit -- Unit type
    | VTNum -- Numbers type
    | VTString -- String type
    | VTFunction ValueType ComputationType -- Function type
    | VTHandler ComputationType ComputationType -- Handler type
    deriving (Show, Eq, Ord)

data ComputationType = CType ValueType [(OperationName, Type, Type)]
    deriving (Show, Eq, Ord)

data Type =
    ValueType ValueType
    | ComputationType ComputationType
    | Void
    deriving (Show, Eq, Ord)

type Env = Map.Map VariableName Type
emptyEnv :: Map.Map VariableName Type
emptyEnv = Map.empty

extendEnv :: VariableName -> Type -> Env -> Env
extendEnv x ty env = Map.insert x ty env
