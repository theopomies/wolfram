module Rules (Rule, Cell (..), RuleNumber (..), getRule) where

import Error
import Control.Exception
import Data.Bits

type Rule = Cell -> Cell -> Cell -> Cell
newtype RuleNumber = RuleNumber Int
newtype Cell = Cell Char
instance Show Cell where
  show (Cell cell) = show cell

rule30 :: Rule
rule30 (Cell '*') (Cell '*') (Cell '*') = Cell ' '
rule30 (Cell '*') (Cell '*') (Cell ' ') = Cell ' '
rule30 (Cell '*') (Cell ' ') (Cell '*') = Cell ' '
rule30 (Cell '*') (Cell ' ') (Cell ' ') = Cell '*'
rule30 (Cell ' ') (Cell '*') (Cell '*') = Cell '*'
rule30 (Cell ' ') (Cell '*') (Cell ' ') = Cell '*'
rule30 (Cell ' ') (Cell ' ') (Cell '*') = Cell '*'
rule30 (Cell ' ') (Cell ' ') (Cell ' ') = Cell ' '
rule30 _          _          _          = throw ExhaustiveException

rule90 :: Rule
rule90 (Cell '*') (Cell '*') (Cell '*') = Cell ' '
rule90 (Cell '*') (Cell '*') (Cell ' ') = Cell '*'
rule90 (Cell '*') (Cell ' ') (Cell '*') = Cell ' '
rule90 (Cell '*') (Cell ' ') (Cell ' ') = Cell '*'
rule90 (Cell ' ') (Cell '*') (Cell '*') = Cell '*'
rule90 (Cell ' ') (Cell '*') (Cell ' ') = Cell ' '
rule90 (Cell ' ') (Cell ' ') (Cell '*') = Cell '*'
rule90 (Cell ' ') (Cell ' ') (Cell ' ') = Cell ' '
rule90 _          _          _          = throw ExhaustiveException

rule110 :: Rule
rule110 (Cell '*') (Cell '*') (Cell '*') = Cell ' '
rule110 (Cell '*') (Cell '*') (Cell ' ') = Cell '*'
rule110 (Cell '*') (Cell ' ') (Cell '*') = Cell '*'
rule110 (Cell '*') (Cell ' ') (Cell ' ') = Cell ' '
rule110 (Cell ' ') (Cell '*') (Cell '*') = Cell '*'
rule110 (Cell ' ') (Cell '*') (Cell ' ') = Cell '*'
rule110 (Cell ' ') (Cell ' ') (Cell '*') = Cell '*'
rule110 (Cell ' ') (Cell ' ') (Cell ' ') = Cell ' '
rule110 _          _          _          = throw ExhaustiveException

getRule :: RuleNumber -> Rule
getRule (RuleNumber 30)  = rule30
getRule (RuleNumber 90)  = rule90
getRule (RuleNumber 110) = rule110
getRule ruleNumber       = constructGenericRule ruleNumber

constructGenericRule :: RuleNumber -> Rule
constructGenericRule rule = genericRule $ getTable rule

getTable :: RuleNumber -> [Cell]
getTable (RuleNumber num) = [genCell num i | i <- [0..7]]

genCell :: Int -> Int -> Cell
genCell rule bitNo
    | testBit rule bitNo = Cell '*'
    | otherwise          = Cell ' '

cellsToIndex :: Cell -> Cell -> Cell -> Int
cellsToIndex (Cell '*') (Cell '*') (Cell '*') = 7
cellsToIndex (Cell '*') (Cell '*') (Cell ' ') = 6
cellsToIndex (Cell '*') (Cell ' ') (Cell '*') = 5
cellsToIndex (Cell '*') (Cell ' ') (Cell ' ') = 4
cellsToIndex (Cell ' ') (Cell '*') (Cell '*') = 3
cellsToIndex (Cell ' ') (Cell '*') (Cell ' ') = 2
cellsToIndex (Cell ' ') (Cell ' ') (Cell '*') = 1
cellsToIndex (Cell ' ') (Cell ' ') (Cell ' ') = 0
cellsToIndex _          _          _          = throw ExhaustiveException

genericRule :: [Cell] -> Cell -> Cell -> Cell -> Cell
genericRule cells c1 c2 c3 = cells !! cellsToIndex c1 c2 c3
