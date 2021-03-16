module TestsDisplay where

import Test.Hspec ( context, describe, it, Spec )
import Parser
    ( WindowMove(WindowMove),
      WindowWidth(WindowWidth),
      WindowLength(WindowLength),
      WindowStart(WindowStart),
      Conf(..) )
import Rules ( Cell(Cell), RuleNumber(RuleNumber), getRule )
import Display ( toScreen )
import Generation ( Lines, Line (Window))

ls :: Lines
ls = [Window [Cell c] | c <- ['a', 'b' .. 'z']]

conf :: Int -> Maybe Int -> Conf
conf start length = Conf (getRule (RuleNumber 30)) (WindowStart start) (WindowLength length) (WindowWidth 0) (WindowMove 0)

displaySpec :: Spec
displaySpec = describe "Display" $ do
  context "Drop" $ do
    it "Should not drop" $ do
      toScreen (conf 0 Nothing) ls == ls
    it "Should drop 1" $ do
      toScreen (conf 1 Nothing) ls == drop 1 ls
    it "Sould drop 10" $ do
      toScreen (conf 10 Nothing) ls == drop 10 ls
    it "Should drop all" $ do
      null(toScreen (conf 27 Nothing) ls)
  context "Take" $ do
    it "Should take all" $ do
      toScreen (conf 0 Nothing) ls == ls
    it "Should take 1" $ do
      toScreen (conf 0 (Just 1)) ls == take 1 ls
    it "Should take 10" $ do
      toScreen (conf 0 (Just 10)) ls == take 10 ls
    it "Should take nothing" $ do
      null(toScreen (conf 0 (Just 0)) ls)
  context "Drop then take" $ do
    it "Should not drop and take everything" $ do
      toScreen (conf 0 Nothing) ls == ls
    it "Should not drop but take 10" $ do 
      toScreen (conf 0 (Just 10)) ls == take 10 ls
    it "Should drop 10 and take everything" $ do
      toScreen (conf 10 Nothing) ls == drop 10 ls
    it "Should drop all and take everything" $ do
      null(toScreen (conf 27 Nothing) ls)
    it "Should not drop and not take" $ do
      null(toScreen (conf 0 (Just 0)) ls)
    it "Should drop everything and not take" $ do
      null(toScreen (conf 27 (Just 0)) ls)