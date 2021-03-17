module Bool.Parsing (BoolNotation, algebraicNotation, boolExpr, programmingNotation) where

import Prelude

import Bool.Model (BoolExpr(..))
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints as CP
import Text.Parsing.StringParser.Expr (Assoc(..), Operator(..), buildExprParser)

type BoolNotation =
  { negationOperator :: String
  , andOperator :: String
  , orOperator :: String
  }

algebraicNotation :: BoolNotation
algebraicNotation =
  { negationOperator: "~"
  , andOperator: "*"
  , orOperator: "+"
  }

programmingNotation :: BoolNotation
programmingNotation =
  { negationOperator: "!"
  , andOperator: "&&"
  , orOperator: "||"
  }

boolExpr' :: BoolNotation -> Parser BoolExpr
boolExpr' cfg = fix $ \p -> buildExprParser opTable (boolTerm p)
  where
  opTable = [ [ Prefix (string cfg.negationOperator $> Not) ]
            , [ Infix (string cfg.andOperator $> And) AssocRight ]
            , [ Infix (string cfg.orOperator $> Or) AssocRight ]
            ]

  boolTerm p = (Var <$> anyLetter)
            <|> char '(' *> p <* char ')'

boolExpr :: BoolNotation -> Parser BoolExpr
boolExpr cfg = CP.skipSpaces *> lexeme (boolExpr' cfg)

-- Utilities for dealing with whitespace

lexeme :: forall a. Parser a -> Parser a
lexeme p = p <* CP.skipSpaces

string :: String -> Parser String
string = lexeme <<< CP.string

anyLetter :: Parser Char
anyLetter = lexeme CP.anyLetter

char :: Char -> Parser Char
char = lexeme <<< CP.char
