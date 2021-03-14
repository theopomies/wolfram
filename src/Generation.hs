module Generation (getLines, Window, Lines) where

import Error ( MyException(ExhaustiveException) )
import Control.Exception ( throw )
import Parser (Conf (..), WindowWidth (..), WindowMove (..))
import Rules (Cell (..), Rule)

type Lines = [Window]
newtype Line = Window [Cell]
instance Show Line where
  show (Window cells) = map cellToChar cells

newtype BeforeWindow = BeforeWindow [Cell]
type Window = Line
newtype AfterWindow = AfterWindow [Cell]
newtype WorkingLine = WorkingLine (BeforeWindow, Window, AfterWindow)

cellToChar :: Cell -> Char
cellToChar (Cell char) = char

getWindow :: WorkingLine -> Window
getWindow (WorkingLine (_, w, _)) = w

getLines :: Conf -> Lines
getLines (Conf rule _ _ windowWidth windowMove) = map getWindow $ getAllLines rule $ firstLine windowWidth windowMove

getAllLines :: Rule -> WorkingLine -> [WorkingLine]
getAllLines rule line = line : getAllLines rule (getNextLine line rule)

beforeWindowIndexes :: [Int]
beforeWindowIndexes = [(-1), (-2)..]

windowIndexes :: WindowWidth -> [Int]
windowIndexes (WindowWidth window) = [0..(window - 1)]

afterWindowIndexes :: WindowWidth -> [Int]
afterWindowIndexes (WindowWidth window) = [window..]

firstLine :: WindowWidth -> WindowMove -> WorkingLine
firstLine window@(WindowWidth windowWidth) (WindowMove move) = let firstNonEmptyCellIndex = move + div windowWidth 2 in
                                                WorkingLine (BeforeWindow [genCell i firstNonEmptyCellIndex | i <- beforeWindowIndexes],
                                                             Window       [genCell i firstNonEmptyCellIndex | i <- windowIndexes window],
                                                             AfterWindow  [genCell i firstNonEmptyCellIndex | i <- afterWindowIndexes window])

genCell :: Int -> Int -> Cell
genCell targetIndex currentIndex
    | targetIndex == currentIndex = Cell '*'
    | otherwise                   = Cell ' '

genWindow :: WorkingLine -> Rule -> Window
genWindow (WorkingLine (BeforeWindow (x:_), Window (a:b:c:as), AfterWindow (y:_))) rule = Window $ rule x a b : rule a b c : genWindow' (Window (b:c:as)) y rule
genWindow (WorkingLine (BeforeWindow (x:_), Window (a:b:_),    AfterWindow (y:_))) rule = Window $ rule x a b : [rule a b y]
genWindow (WorkingLine (BeforeWindow (x:_), Window (a:_),      AfterWindow (y:_))) rule = Window [rule x a y]
genWindow (WorkingLine (_,                  Window [],        _))                  _    = Window []
genWindow _                                                                        _    = throw ExhaustiveException

genWindow' :: Window -> Cell -> Rule -> [Cell]
genWindow' (Window (x:y:z:xs)) cell rule = rule x y z : genWindow' (Window (y:z:xs)) cell rule
genWindow' (Window (x:y:_))    cell rule = [rule x y cell]
genWindow' _                   _    _    = throw ExhaustiveException

genBefore :: WorkingLine -> Rule -> BeforeWindow
genBefore line rule = BeforeWindow $ genBefore' line rule

genBefore' :: WorkingLine -> Rule -> [Cell]
genBefore' (WorkingLine (BeforeWindow (x:y:z:xs), Window [],     AfterWindow []))     rule = rule z y x : genBefore' (WorkingLine (BeforeWindow (y:z:xs), Window [], AfterWindow [])) rule
genBefore' (WorkingLine (BeforeWindow (x:y:xs),   Window [],     AfterWindow (z:_)))  rule = rule y x z : genBefore' (WorkingLine (BeforeWindow (x:y:xs), Window [], AfterWindow [])) rule
genBefore' (WorkingLine (BeforeWindow (x:y:xs),   Window (z:_),  _))                  rule = rule y x z : genBefore' (WorkingLine (BeforeWindow (x:y:xs), Window [], AfterWindow [])) rule
genBefore' _                                                                          _    = throw ExhaustiveException

genAfter :: WorkingLine -> Rule -> AfterWindow
genAfter line rule = AfterWindow $ genAfter' line rule

genAfter' :: WorkingLine -> Rule -> [Cell]
genAfter' (WorkingLine (BeforeWindow [],    Window [],          AfterWindow (x:y:z:xs))) rule = rule x y z : genAfter' (WorkingLine (BeforeWindow [], Window [], AfterWindow (y:z:xs))) rule
genAfter' (WorkingLine (BeforeWindow (x:_), Window [],          AfterWindow (y:z:ys)))   rule = rule x y z : genAfter' (WorkingLine (BeforeWindow [], Window [], AfterWindow (y:z:ys))) rule
genAfter' (WorkingLine (_,                  Window list@(_:_),  AfterWindow (y:z:ys)))   rule = rule (last list) y z : genAfter' (WorkingLine (BeforeWindow [], Window [], AfterWindow (y:z:ys))) rule
genAfter' _                                                                               _    = throw ExhaustiveException

getNextLine :: WorkingLine -> Rule -> WorkingLine
getNextLine line rule = WorkingLine (genBefore line rule, genWindow line rule, genAfter line rule)