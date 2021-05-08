module Bool.SumOfProducts where

import Prelude

import Bool.Eval (truthTable)
import Bool.Model (BoolExpr(..))
import Data.Array (filter, zip)
import Data.Array.NonEmpty (foldr1, fromArray)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)
  

-- TODO: really bad, but it works
sumOfProducts :: BoolExpr -> BoolExpr
sumOfProducts expr = foldr1 Or (toNonEmpty bar)
  where
  table = truthTable expr
  foo = filter _.value table.rows
  bar = map (\row -> foldr1 And (toNonEmpty $ map f (zip table.variables row.bindings))) foo :: Array BoolExpr
  f :: Tuple Char Boolean -> BoolExpr
  f (var /\ true) = Var var
  f (var /\ false) = Not $ Var var
  
  toNonEmpty x = unsafePartial $ fromJust $ fromArray x
