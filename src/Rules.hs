module Rules (Rule, Cell (..), rule30, rule90, rule110) where

import Error
import Control.Exception

type Rule = Cell -> Cell -> Cell -> Cell
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
