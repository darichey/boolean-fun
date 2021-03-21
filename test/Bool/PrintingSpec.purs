module Test.Spec.PrintingSpec where

import Prelude

import Bool.Model (BoolExpr(..), prettyPrint)
import Bool.Notation (algebraicNotation, programmingNotation)
import Data.Foldable (traverse_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type TestConf =
  { name :: String
  , input :: BoolExpr
  , expected :: { algebraic :: String, programming :: String }
  }

tests :: Array TestConf
tests =
  [ { name: "single variable"
    , input: (Var 'a')
    , expected:
      { algebraic: "a"
      , programming: "a"
      }
    }
  , { name: "NOT expression"
    , input: (Not (Var 'a'))
    , expected:
      { algebraic: "~a"
      , programming: "!a"
      }
    }
  , { name: "AND expression"
    , input: (And (Var 'a') (Var 'b'))
    , expected:
      { algebraic: "a * b"
      , programming: "a && b"
      }
    }
  , { name: "OR expression"
    , input: (Or (Var 'a') (Var 'b'))
    , expected:
      { algebraic: "a + b"
      , programming: "a || b"
      }
    }
  , { name: "AND associativity 1"
    , input: (And (And (Var 'a') (Var 'b')) (Var 'c'))
    , expected:
      { algebraic: "a * b * c"
      , programming: "a && b && c"
      }
    }
  , { name: "AND associativity 2"
    , input: (And (Var 'a') (And (Var 'b') (Var 'c')))
    , expected:
      { algebraic: "a * b * c"
      , programming: "a && b && c"
      }
    }
  , { name: "OR associativity 1"
    , input: (Or (Or (Var 'a') (Var 'b')) (Var 'c'))
    , expected:
      { algebraic: "a + b + c"
      , programming: "a || b || c"
      }
    }
  , { name: "OR associativity 2"
    , input: (Or (Var 'a') (Or (Var 'b') (Var 'c')))
    , expected:
      { algebraic: "a + b + c"
      , programming: "a || b || c"
      }
    }
  , { name: "nested NOT"
    , input: (Not (Not (Var 'a')))
    , expected:
      { algebraic: "~(~a)"
      , programming: "!(!a)"
      }
    }
  , { name: "complex expression"
    , input: (Or (Or (Not (Var 'a')) (Not (And (Var 'c') (Var 'd')))) (And (Var 'a') (And (Not (Var 'b')) (Var 'c'))))
    , expected:
      { algebraic: "~a + ~(c * d) + a * ~b * c"
      , programming: "!a || !(c && d) || a && !b && c"
      }
    }
  ]

spec :: Spec Unit
spec =
  describe "PrintingSpec" do
    doTests "algebraic notation" algebraicNotation _.algebraic
    doTests "programming notation" programmingNotation _.programming

    where
    doTest notation getExpected test =
      it test.name do
        let res = prettyPrint notation test.input
        res `shouldEqual` (getExpected test.expected)

    doTests name notation getInput =
      describe name do
        traverse_ (doTest notation getInput) tests