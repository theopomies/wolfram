module TestsRules where

import Test.Hspec ( context, describe, it, Spec, shouldThrow )
import Rules (rule30, rule90, rule110, Cell (..), Rule, RuleNumber (..), constructGenericRule)
import Error (MyException (ExhaustiveException))
import Control.Exception ( evaluate )

genRule90 :: Rule
genRule90 = constructGenericRule (RuleNumber 90)

rulesSpec :: Spec
rulesSpec = describe "Rules" $ do
  context "Rule 30" $ do
    it "'***' -> ' '" $ do
      rule30 (Cell '*') (Cell '*') (Cell '*') == Cell ' '
    it "'** ' -> ' '" $ do
      rule30 (Cell '*') (Cell '*') (Cell ' ') == Cell ' '
    it "'* *' -> ' '" $ do
      rule30 (Cell '*') (Cell ' ') (Cell '*') == Cell ' '
    it "'*  ' -> '*'" $ do
      rule30 (Cell '*') (Cell ' ') (Cell ' ') == Cell '*'
    it "' **' -> '*'" $ do
      rule30 (Cell ' ') (Cell '*') (Cell '*') == Cell '*'
    it "' * ' -> '*'" $ do
      rule30 (Cell ' ') (Cell '*') (Cell ' ') == Cell '*'
    it "'  *' -> '*'" $ do
      rule30 (Cell ' ') (Cell ' ') (Cell '*') == Cell '*'
    it "'   ' -> ' '" $ do
      rule30 (Cell ' ') (Cell ' ') (Cell ' ') == Cell ' '
    it "Should throw on invalid cell 1" $ do
      evaluate (rule30 (Cell 'a') (Cell ' ') (Cell ' ')) `shouldThrow` (== ExhaustiveException)
    it "Should throw on invalid cell 2" $ do
      evaluate (rule30 (Cell ' ') (Cell 'a') (Cell ' ')) `shouldThrow` (== ExhaustiveException)
    it "Should throw on invalid cell 3" $ do
      evaluate (rule30 (Cell ' ') (Cell ' ') (Cell 'a')) `shouldThrow` (== ExhaustiveException)
    it "Should throw on invalid cell 4" $ do
      evaluate (rule30 (Cell 'a') (Cell ' ') (Cell 'a')) `shouldThrow` (== ExhaustiveException)
  context "Rule 90" $ do
    it "'***' -> ' '" $ do
      rule90 (Cell '*') (Cell '*') (Cell '*') == Cell ' '
    it "'** ' -> '*'" $ do
      rule90 (Cell '*') (Cell '*') (Cell ' ') == Cell '*'
    it "'* *' -> ' '" $ do
      rule90 (Cell '*') (Cell ' ') (Cell '*') == Cell ' '
    it "'*  ' -> '*'" $ do
      rule90 (Cell '*') (Cell ' ') (Cell ' ') == Cell '*'
    it "' **' -> '*'" $ do
      rule90 (Cell ' ') (Cell '*') (Cell '*') == Cell '*'
    it "' * ' -> ' '" $ do
      rule90 (Cell ' ') (Cell '*') (Cell ' ') == Cell ' '
    it "'  *' -> '*'" $ do
      rule90 (Cell ' ') (Cell ' ') (Cell '*') == Cell '*'
    it "'   ' -> ' '" $ do
      rule90 (Cell ' ') (Cell ' ') (Cell ' ') == Cell ' '
    it "Should throw on invalid cell 1" $ do
      evaluate (rule90 (Cell 'a') (Cell ' ') (Cell ' ')) `shouldThrow` (== ExhaustiveException)
    it "Should throw on invalid cell 2" $ do
      evaluate (rule90 (Cell ' ') (Cell 'a') (Cell ' ')) `shouldThrow` (== ExhaustiveException)
    it "Should throw on invalid cell 3" $ do
      evaluate (rule90 (Cell ' ') (Cell ' ') (Cell 'a')) `shouldThrow` (== ExhaustiveException)
    it "Should throw on invalid cell 4" $ do
      evaluate (rule90 (Cell 'a') (Cell ' ') (Cell 'a')) `shouldThrow` (== ExhaustiveException)
  context "Rule 110" $ do
    it "'***' -> ' '" $ do
      rule110 (Cell '*') (Cell '*') (Cell '*') == Cell ' '
    it "'** ' -> '*'" $ do
      rule110 (Cell '*') (Cell '*') (Cell ' ') == Cell '*'
    it "'* *' -> '*'" $ do
      rule110 (Cell '*') (Cell ' ') (Cell '*') == Cell '*'
    it "'*  ' -> ' '" $ do
      rule110 (Cell '*') (Cell ' ') (Cell ' ') == Cell ' '
    it "' **' -> '*'" $ do
      rule110 (Cell ' ') (Cell '*') (Cell '*') == Cell '*'
    it "' * ' -> '*'" $ do
      rule110 (Cell ' ') (Cell '*') (Cell ' ') == Cell '*'
    it "'  *' -> '*'" $ do
      rule110 (Cell ' ') (Cell ' ') (Cell '*') == Cell '*'
    it "'   ' -> ' '" $ do
      rule110 (Cell ' ') (Cell ' ') (Cell ' ') == Cell ' '
    it "Should throw on invalid cell 1" $ do
      evaluate (rule110 (Cell 'a') (Cell ' ') (Cell ' ')) `shouldThrow` (== ExhaustiveException)
    it "Should throw on invalid cell 2" $ do
      evaluate (rule110 (Cell ' ') (Cell 'a') (Cell ' ')) `shouldThrow` (== ExhaustiveException)
    it "Should throw on invalid cell 3" $ do
      evaluate (rule110 (Cell ' ') (Cell ' ') (Cell 'a')) `shouldThrow` (== ExhaustiveException)
    it "Should throw on invalid cell 4" $ do
      evaluate (rule110 (Cell 'a') (Cell ' ') (Cell 'a')) `shouldThrow` (== ExhaustiveException)
  context "Generic rule 90" $ do
    it "'***' -> ' '" $ do
      genRule90 (Cell '*') (Cell '*') (Cell '*') == Cell ' '
    it "'** ' -> '*'" $ do
      genRule90 (Cell '*') (Cell '*') (Cell ' ') == Cell '*'
    it "'* *' -> ' '" $ do
      genRule90 (Cell '*') (Cell ' ') (Cell '*') == Cell ' '
    it "'*  ' -> '*'" $ do
      genRule90 (Cell '*') (Cell ' ') (Cell ' ') == Cell '*'
    it "' **' -> '*'" $ do
      genRule90 (Cell ' ') (Cell '*') (Cell '*') == Cell '*'
    it "' * ' -> ' '" $ do
      genRule90 (Cell ' ') (Cell '*') (Cell ' ') == Cell ' '
    it "'  *' -> '*'" $ do
      genRule90 (Cell ' ') (Cell ' ') (Cell '*') == Cell '*'
    it "'   ' -> ' '" $ do
      genRule90 (Cell ' ') (Cell ' ') (Cell ' ') == Cell ' '
    it "Should throw on invalid cell 1" $ do
      evaluate (genRule90 (Cell 'a') (Cell ' ') (Cell ' ')) `shouldThrow` (== ExhaustiveException)
    it "Should throw on invalid cell 2" $ do
      evaluate (genRule90 (Cell ' ') (Cell 'a') (Cell ' ')) `shouldThrow` (== ExhaustiveException)
    it "Should throw on invalid cell 3" $ do
      evaluate (genRule90 (Cell ' ') (Cell ' ') (Cell 'a')) `shouldThrow` (== ExhaustiveException)
    it "Should throw on invalid cell 4" $ do
      evaluate (genRule90 (Cell 'a') (Cell ' ') (Cell 'a')) `shouldThrow` (== ExhaustiveException)