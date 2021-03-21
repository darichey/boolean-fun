module Bool.Notation (BoolNotation, algebraicNotation, programmingNotation) where

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