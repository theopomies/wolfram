module TestsLexer where

import Lexer
    ( TOKEN(Value, RULE, START, LINES, WINDOW, MOVE), tokenize )
import Test.Hspec ( describe, it, shouldThrow, Spec ) 
import Error ( MyException (..)) 
import Control.Exception (evaluate)

lexerSpec :: Spec
lexerSpec = describe "Lexer" $ do
  it "Should return an empty array" $ do
    null $ tokenize []
  it "Should return a rule token" $ do
    tokenize ["--rule"] == [RULE]
  it "Should return a start token" $ do
    tokenize ["--start"] == [START]
  it "Should return a lines token" $ do
    tokenize ["--lines"] == [LINES]
  it "Should return a window token" $ do
    tokenize ["--window"] == [WINDOW]
  it "Should return a move token" $ do
    tokenize ["--move"] == [MOVE]
  it "Should return a value token" $ do
    tokenize ["test"] == [Value "test"]
  it "Should throw a UsageException" $ do
    evaluate (tokenize ["-h"]) `shouldThrow` (== UsageException)