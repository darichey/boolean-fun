module Test.Spec.ParsingSpec where

import Prelude

import Bool.Model (BoolExpr(..))
import Bool.Parsing (BoolNotation, algebraicNotation, boolExpr, programmingNotation)
import Data.Either (Either(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (runParser)

spec :: Spec Unit
spec =
  describe "ParsingSpec" do
    describe "algebraic notation" do
      it "parses a single variable" do
        let input = "a"
        let res = runParser (boolExpr algebraicNotation) input
        res `shouldEqual` (Right (Var 'a'))

      it "parses a NOT expression" do
        let input = "~a"
        let res = runParser (boolExpr algebraicNotation) input
        res `shouldEqual` (Right (Not (Var 'a')))

      it "parses an AND expression" do
        let input = "a * b"
        let res = runParser (boolExpr algebraicNotation) input
        res `shouldEqual` (Right (And (Var 'a') (Var 'b')))

      it "parses an OR expression" do
        let input = "a + b"
        let res = runParser (boolExpr algebraicNotation) input
        res `shouldEqual` (Right (Or (Var 'a') (Var 'b')))

    describe "programming notation" do
      it "parses a single variable" do
        let input = "a"
        let res = runParser (boolExpr programmingNotation) input
        res `shouldEqual` (Right (Var 'a'))

      it "parses a NOT expression" do
        let input = "!a"
        let res = runParser (boolExpr programmingNotation) input
        res `shouldEqual` (Right (Not (Var 'a')))

      it "parses an AND expression" do
        let input = "a && b"
        let res = runParser (boolExpr programmingNotation) input
        res `shouldEqual` (Right (And (Var 'a') (Var 'b')))

      it "parses an OR expression" do
        let input = "a || b"
        let res = runParser (boolExpr programmingNotation) input
        res `shouldEqual` (Right (Or (Var 'a') (Var 'b')))
