module Generation where

import Error
import Control.Exception
import Libs

rule30 :: Rule
rule30 '*' '*' '*' = ' '
rule30 '*' '*' ' ' = ' '
rule30 '*' ' ' '*' = ' '
rule30 '*' ' ' ' ' = '*'
rule30 ' ' '*' '*' = '*'
rule30 ' ' '*' ' ' = '*'
rule30 ' ' ' ' '*' = '*'
rule30 ' ' ' ' ' ' = ' '
rule30 _   _   _   = throw ExhaustiveException

rule90 :: Rule
rule90 '*' '*' '*' = ' '
rule90 '*' '*' ' ' = '*'
rule90 '*' ' ' '*' = ' '
rule90 '*' ' ' ' ' = '*'
rule90 ' ' '*' '*' = '*'
rule90 ' ' '*' ' ' = ' '
rule90 ' ' ' ' '*' = '*'
rule90 ' ' ' ' ' ' = ' '
rule90 _   _   _   = throw ExhaustiveException

rule110 :: Rule
rule110 '*' '*' '*' = ' '
rule110 '*' '*' ' ' = '*'
rule110 '*' ' ' '*' = '*'
rule110 '*' ' ' ' ' = ' '
rule110 ' ' '*' '*' = '*'
rule110 ' ' '*' ' ' = '*'
rule110 ' ' ' ' '*' = '*'
rule110 ' ' ' ' ' ' = ' '
rule110 _   _   _   = throw ExhaustiveException

genList :: Int -> Int -> Char
genList a b
    | a == b    = '*'
    | otherwise = ' '

firstLine :: EndConf -> (EndConf, Line)
firstLine conf@(EndConf _ _ _ (WindowNumber window) (Move move)) = (conf, ([genList i ((-1) * move - div window 2 - 1) | i <- [0..]],
                                                                           [genList i (move + div window 2) | i <- [0..(window - 1)]],
                                                                           [genList i (move - div window 2 - mod window 2) | i <- [0..]]))

genWindow :: Line -> Rule -> Window
genWindow ((x:xs), (a:b:c:as), (y:ys)) rule = rule x a b : rule a b c : genWindow' (b:c:as) y rule
genWindow ((x:xs), (a:b:as),   (y:ys)) rule = rule x a b : [rule a b y]
genWindow ((x:xs), (a:as),     (y:ys)) rule = [rule x a y]
genWindow (_,      [],         _)      _    = []
genWindow _                            _    = throw ExhaustiveException

genWindow' :: Window -> Cell -> Rule -> Window
genWindow' (x:y:z:xs) cell rule = rule x y z : genWindow' (y:z:xs) cell rule
genWindow' (x:y:xs)   cell rule = [rule x y cell]
genWindow' _          _    _    = throw ExhaustiveException

genBefore :: Line -> Rule -> Window
genBefore ((x:y:z:xs), [], [])     rule = rule z y x : genBefore (y:z:xs, [], []) rule
genBefore ((x:y:xs),   [], (z:zs)) rule = rule y x z : genBefore (x:y:xs, [], []) rule
genBefore ((x:y:xs),   (z:zs), _)  rule = rule y x z : genBefore (x:y:xs, [], []) rule
genBefore _                        _    = throw ExhaustiveException

genAfter :: Line -> Rule -> Window
genAfter ([],     [],     (x:y:z:xs))          rule = rule x y z : genAfter ([], [], y:z:xs) rule
genAfter ((x:xs), [],     (y:z:ys))            rule = rule x y z : genAfter ([], [], y:z:ys) rule
genAfter (_,      list@(x:xs), (y:z:ys))       rule = rule (last list) y z : genAfter ([], [], y:z:ys) rule
genAfter _                                     _    = throw ExhaustiveException

getNextLine :: Line -> Rule -> Line
getNextLine line rule = (genBefore line rule, genWindow line rule, genAfter line rule)

getAllLines :: (EndConf, Line) -> [Line]
getAllLines (conf@(EndConf rule _ _ _ _), line) = line : getAllLines (conf, getNextLine line rule)

getLines :: (EndConf, Line) -> (EndConf, [Line])
getLines tuple@(conf, _) = (conf, getAllLines tuple)