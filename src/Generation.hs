module Generation where

import Error
import Control.Exception
import Libs

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

genList :: Int -> Int -> Cell
genList a b
    | a == b    = Cell '*'
    | otherwise = Cell ' '

firstLine :: EndConf -> (EndConf, Line)
firstLine conf@(EndConf _ _ _ (WindowNumber window) (Move move)) = (conf, Line (BeforeWindow [genList i ((-1) * move - div window 2 - 1) | i <- [0..]],
                                                                           Window [genList i (move + div window 2) | i <- [0..(window - 1)]],
                                                                           AfterWindow [genList i (move - div window 2 - mod window 2) | i <- [0..]]))

genWindow :: Line -> Rule -> Window
genWindow (Line (BeforeWindow (x:xs), Window (a:b:c:as), AfterWindow (y:ys))) rule = Window $ rule x a b : rule a b c : genWindow' (Window (b:c:as)) y rule
genWindow (Line (BeforeWindow (x:xs), Window (a:b:as),   AfterWindow (y:ys))) rule = Window $ rule x a b : [rule a b y]
genWindow (Line (BeforeWindow (x:xs), Window (a:as),     AfterWindow (y:ys))) rule = Window [rule x a y]
genWindow (Line (_,                   Window [],         _))                  _    = Window []
genWindow _                                                                   _    = throw ExhaustiveException

genWindow' :: Window -> Cell -> Rule -> [Cell]
genWindow' (Window (x:y:z:xs)) cell rule = rule x y z : genWindow' (Window (y:z:xs)) cell rule
genWindow' (Window (x:y:xs))   cell rule = [rule x y cell]
genWindow' _                   _    _    = throw ExhaustiveException

genBefore :: Line -> Rule -> BeforeWindow
genBefore line rule = BeforeWindow $ genBefore' line rule

genBefore' :: Line -> Rule -> [Cell]
genBefore' (Line (BeforeWindow (x:y:z:xs), Window [],     AfterWindow []))     rule = rule z y x : genBefore' (Line (BeforeWindow (y:z:xs), Window [], AfterWindow [])) rule
genBefore' (Line (BeforeWindow (x:y:xs),   Window [],     AfterWindow (z:zs))) rule = rule y x z : genBefore' (Line (BeforeWindow (x:y:xs), Window [], AfterWindow [])) rule
genBefore' (Line (BeforeWindow (x:y:xs),   Window (z:zs), _))                  rule = rule y x z : genBefore' (Line (BeforeWindow (x:y:xs), Window [], AfterWindow [])) rule
genBefore' _                                                                   _    = throw ExhaustiveException

genAfter :: Line -> Rule -> AfterWindow
genAfter line rule = AfterWindow $ genAfter' line rule

genAfter' :: Line -> Rule -> [Cell]
genAfter' (Line (BeforeWindow [],     Window [],          AfterWindow (x:y:z:xs))) rule = rule x y z : genAfter' (Line (BeforeWindow [], Window [], AfterWindow (y:z:xs))) rule
genAfter' (Line (BeforeWindow (x:xs), Window [],          AfterWindow (y:z:ys)))   rule = rule x y z : genAfter' (Line (BeforeWindow [], Window [], AfterWindow (y:z:ys))) rule
genAfter' (Line (_,                   Window list@(x:xs), AfterWindow (y:z:ys)))   rule = rule (last list) y z : genAfter' (Line (BeforeWindow [], Window [], AfterWindow (y:z:ys))) rule
genAfter' _                                                                        _    = throw ExhaustiveException

getNextLine :: Line -> Rule -> Line
getNextLine line rule = Line (genBefore line rule, genWindow line rule, genAfter line rule)

getAllLines :: (EndConf, Line) -> [Line]
getAllLines (conf@(EndConf rule _ _ _ _), line) = line : getAllLines (conf, getNextLine line rule)

getLines :: (EndConf, Line) -> (EndConf, [Line])
getLines tuple@(conf, _) = (conf, getAllLines tuple)