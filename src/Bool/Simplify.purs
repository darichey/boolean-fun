module Bool.Simplify where

import Prelude

import Bool.Model (BoolExpr)

simplify :: BoolExpr -> BoolExpr
simplify = identity
