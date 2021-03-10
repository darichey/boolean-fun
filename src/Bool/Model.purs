module Bool.Model (BoolExpr(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data BoolExpr
  = Var Char
  | Not BoolExpr
  | And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr

derive instance genericBoolExpr :: Generic BoolExpr _

instance showBoolExpr :: Show BoolExpr where
  show x = genericShow x
