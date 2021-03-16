module TestsParser where

import Parser
import Rules (RuleNumber (RuleNumber))
import Test.Hspec
import Error
import Control.Exception
import Lexer
import Error
import Test.Hspec.QuickCheck

parserSpec :: Spec
parserSpec = describe "Parser" $ do
  prop "Should set rule" $ 
    \x -> setRule defaultConf x == WorkingConf (Just (RuleNumber x)) defaultStart defaultLength defaultWidth defaultMove
  prop "Should set start" $
    \x -> setWindowStart defaultConf x == WorkingConf defaultRule (WindowStart x) defaultLength defaultWidth defaultMove
  prop "Should set length" $
    \x -> setWindowLength defaultConf x == WorkingConf defaultRule defaultStart (WindowLength (Just x)) defaultWidth defaultMove
  prop "Should set width" $
    \x -> setWindowWidth defaultConf x == WorkingConf defaultRule defaultStart defaultLength (WindowWidth x) defaultMove
  prop "Should set move" $
    \x -> setWindowMove defaultConf x == WorkingConf defaultRule defaultStart defaultLength defaultWidth (WindowMove x)
  it "Should read a valid start" $ do
    readWindowStart "10" == 10
  it "Should throw on negative start" $ do
    evaluate (readWindowStart "-10") `shouldThrow` (== ArgException)
  it "Should throw on non-numerical start" $ do
    evaluate (readWindowStart "start") `shouldThrow` (== ArgException)
  it "Should read a valid length" $ do
    readWindowLength "10" == 10
  it "Should throw on negativelength" $ do
    evaluate (readWindowLength "-10") `shouldThrow` (== ArgException)
  it "Should throw on non-numerical length" $ do
    evaluate (readWindowLength "length") `shouldThrow` (== ArgException)
  it "Should read a valid rule" $ do
    readRule "10" == 10
  it "Should throw on negative rule" $ do
    evaluate (readRule "-10") `shouldThrow` (== ArgException)
  it "Should throw on rule greater than 255" $ do
    evaluate (readRule "256") `shouldThrow` (== ArgException)
  it "Should throw on non-numerical rule" $ do
    evaluate (readRule "rule") `shouldThrow` (== ArgException)
  it "Should read a valid width" $ do
    readWindowWidth "10" == 10
  it "Should throw on negative width" $ do
    evaluate (readWindowWidth "-10") `shouldThrow` (== ArgException)
  it "Should throw on non-numerical width" $ do
    evaluate (readWindowWidth "width") `shouldThrow` (== ArgException)
  it "Should read a valid positive move" $ do
    readWindowMove "10" == 10
  it "Should read a valid negative move" $ do
    readWindowMove "-10" == -10
  it "Should throw on non-numerical move" $ do
    evaluate (readWindowMove "move") `shouldThrow` (== ArgException)
  it "Should parse a list of valid tokens correctly" $ do
    parse [RULE, Value "30"] == toConf (WorkingConf (Just (RuleNumber 30)) defaultStart defaultLength defaultWidth defaultMove)
  it "Should parse a list of valid complete tokens correctly" $ do
    parse [RULE, Value "30", START, Value "10", MOVE, Value "20", LINES, Value "40", WINDOW, Value "50"] == toConf (WorkingConf (Just (RuleNumber 30)) (WindowStart 10) (WindowLength (Just 40)) (WindowWidth 50) (WindowMove 20))
  it "Should throw if a token isnt followed by a value" $ do
    evaluate (parse [RULE, START]) `shouldThrow` (== ArgException)