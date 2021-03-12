module Generation where

import Error
import Control.Exception
import Libs

rule30 :: Rule
rule30 _   _   _   = throw ExhaustiveException
rule30 '*' '*' '*' = ' '
rule30 '*' '*' ' ' = ' '
rule30 '*' ' ' '*' = ' '
rule30 '*' ' ' ' ' = '*'
rule30 ' ' '*' '*' = '*'
rule30 ' ' '*' ' ' = '*'
rule30 ' ' ' ' '*' = '*'
rule30 ' ' ' ' ' ' = ' '

rule90 :: Rule
rule90 _   _   _   = throw ExhaustiveException
rule90 '*' '*' '*' = ' '
rule90 '*' '*' ' ' = '*'
rule90 '*' ' ' '*' = ' '
rule90 '*' ' ' ' ' = '*'
rule90 ' ' '*' '*' = '*'
rule90 ' ' '*' ' ' = ' '
rule90 ' ' ' ' '*' = '*'
rule90 ' ' ' ' ' ' = ' '

rule110 :: Rule
rule110 _   _   _   = throw ExhaustiveException
rule110 '*' '*' '*' = ' '
rule110 '*' '*' ' ' = '*'
rule110 '*' ' ' '*' = '*'
rule110 '*' ' ' ' ' = ' '
rule110 ' ' '*' '*' = '*'
rule110 ' ' '*' ' ' = '*'
rule110 ' ' ' ' '*' = '*'
rule110 ' ' ' ' ' ' = ' '

genList :: Int -> Int -> Char
genList a b
    | a == b = '*'
    | otherwise = 'a'

firstLine :: EndConf -> Line
firstLine (EndConf _ _ _ (WindowNumber window) (Move move)) = ([genList i ((-1) * move - div window 2 - 1) | i <- [0..]],
                                                               [genList i (move + div window 2) | i <- [0..(window - 1)]],
                                                               [genList i (move - div window 2 - mod window 2) | i <- [0..]])