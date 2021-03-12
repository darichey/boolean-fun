module Bool.Parsing (boolExpr) where

import Prelude

import Bool.Model (BoolExpr(..))
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (anyLetter, char, string)
import Text.Parsing.StringParser.Expr (Assoc(..), Operator(..), buildExprParser)

boolExpr :: Parser BoolExpr
boolExpr = fix $ \p -> buildExprParser opTable (boolTerm p)
  where
  opTable = [ [ Prefix (string "~" $> Not) ]
            , [ Infix (string "*" $> And) AssocRight ]
            , [ Infix (string "+" $> Or) AssocRight ]
            ]

  boolTerm p = (Var <$> anyLetter)
            <|> char '(' *> p <* char ')'
