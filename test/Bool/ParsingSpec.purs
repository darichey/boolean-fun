module Test.Spec.ParsingSpec where

import Prelude

import Bool.Model (BoolExpr(..))
import Bool.Notation (algebraicNotation, programmingNotation)
import Bool.Parsing (boolExpr)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Traversable (traverse_)
import Effect.Exception (Error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (ParseError, runParser)

type TestConf a m =
  { name :: String 
  , input :: { algebraic :: String, programming :: String }
  , expected :: a -> m Unit
  }

tests :: forall m. MonadThrow Error m => Array (TestConf (Either ParseError BoolExpr) m)
tests =
  [ { name: "parses a single variable"
    , input:
        { algebraic: "a"
        , programming: "a"
        }
    , expected: (_ `shouldEqual` (Right (Var 'a')))
    }
  , { name: "parses a NOT expression"
    , input:
        { algebraic: "~a"
        , programming: "!a"
        }
    , expected: (_ `shouldEqual` (Right (Not (Var 'a'))))
    }
  , { name: "parses an AND expression"
    , input:
        { algebraic: "a * b"
        , programming: "a && b"
        }
    , expected: (_ `shouldEqual` (Right (And (Var 'a') (Var 'b'))))
    }
  , { name: "parses an OR expression"
    , input:
        { algebraic: "a + b"
        , programming: "a || b"
        }
    , expected: (_ `shouldEqual` (Right (Or (Var 'a') (Var 'b'))))
    }
  , { name: "parses a complex expression"
    , input:
        { algebraic: "(a + (~b * c)) * ~d"
        , programming: "(a || (!b && c)) && !d"
        }
    , expected: (_ `shouldEqual` (Right (And (Or (Var 'a') (And (Not (Var 'b')) (Var 'c'))) (Not (Var 'd')))))
    }
  , { name: "parses with extraneous whitespace"
    , input:
        { algebraic: "   (a \t  + ( ~ b   *\n\n  c)     ) * ~d    "
        , programming: "   (a \t  || ( ! b   &&\n\n  c)     ) && !d    "
        }
    , expected: (_ `shouldEqual` (Right (And (Or (Var 'a') (And (Not (Var 'b')) (Var 'c'))) (Not (Var 'd')))))
    }
  ]

spec :: Spec Unit
spec =
  describe "ParsingSpec" do
    doTests "algebraic notation" algebraicNotation _.algebraic
    doTests "programming notation" programmingNotation _.programming

    where
    doTest notation getInput test =
      it test.name do
        let res = runParser (boolExpr notation) (getInput test.input)
        test.expected res

    doTests name notation getInput =
      describe name do
        traverse_ (doTest notation getInput) tests
