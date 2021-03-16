module TestsGeneration where

import Test.Hspec
import Generation
import Rules ( Cell(Cell) )
import Test.Hspec.QuickCheck

generationSpec :: Spec
generationSpec = describe "Generation" $ do
  prop "cellToChar should extract a char from a Cell" $
    \x -> cellToChar (Cell x) == x
  prop "genCell should return * on a == b and ' ' else" $
    \ x y -> genCell x y == (if x == y then Cell '*' else Cell ' ')