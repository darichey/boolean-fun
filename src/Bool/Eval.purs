module Bool.Eval where

import Prelude

import Bool.Model (BoolExpr(..))
import Data.Array (cons, length, sort, zip)
import Data.Map as M
import Data.Maybe (fromJust)
import Data.Set as S
import Data.Unfoldable (replicateA)
import Partial.Unsafe (unsafePartial)

type Bindings = M.Map Char Boolean

type TruthTable =
  { variables :: Array Char
  , rows :: Array { bindings :: Array Boolean, value :: Boolean }
  }

-- Evaluate a boolean expression given the values for every variable.
-- Partial if not given bindings for every variable
eval :: Partial => Bindings -> BoolExpr -> Boolean
eval bindings = go
  where
  go :: Partial => BoolExpr -> Boolean
  go = case _ of
    Var c -> fromJust $ M.lookup c bindings
    Not e -> not $ go e
    And e1 e2 -> (go e1) && (go e2)
    Or e1 e2 -> (go e1) || (go e2)

-- Get all of the variables in a boolean expression (in sorted order)
vars :: BoolExpr -> Array Char
vars = sort <<< S.toUnfoldable <<< go S.empty
  where
  go :: S.Set Char -> BoolExpr -> S.Set Char
  go acc = case _ of
    Var c -> S.insert c acc
    Not e -> go acc e
    And e1 e2 -> S.union (go acc e1) (go acc e2)
    Or e1 e2 -> S.union (go acc e1) (go acc e2)

truthTable :: BoolExpr -> TruthTable
truthTable expr =
  { variables
  , rows: map (\bindings -> { bindings, value: unsafePartial $ eval (mkBindings variables bindings) expr }) (bools (length variables))
  }
  where
    variables = vars expr
  
bools :: Int -> Array (Array Boolean)
bools n = replicateA n [true, false]

mkBindings :: Array Char -> Array Boolean -> Bindings
mkBindings vars values = M.fromFoldable $ zip vars values