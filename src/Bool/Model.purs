module Bool.Model (BoolExpr(..), prettyPrint) where

import Prelude

import Bool.Notation (BoolNotation)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (singleton)

data BoolExpr
  = Var Char
  | Not BoolExpr
  | And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr

derive instance eqBoolExpr :: Eq BoolExpr

derive instance genericBoolExpr :: Generic BoolExpr _

instance showBoolExpr :: Show BoolExpr where
  show x = genericShow x

prettyPrint :: BoolNotation -> BoolExpr -> String
prettyPrint { negationOperator, andOperator, orOperator } = go 0
  where
  go :: Int -> BoolExpr -> String
  go outerPrec = case _ of
    Var c -> singleton c
    Not e -> showParen (outerPrec > 3) $ negationOperator <> go 4 e
    And e1 e2 -> showParen (outerPrec > 2) $ go 2 e1 <> " " <> andOperator <> " " <> go 2 e2
    Or e1 e2 -> showParen (outerPrec > 1) $ go 1 e1 <> " " <> orOperator <> " " <> go 1 e2

  showParen :: Boolean -> String -> String
  showParen b p = if b then "(" <> p <> ")" else p
