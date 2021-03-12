module Libs where

type Cell = Char
type Rule = Cell -> Cell -> Cell -> Cell
type BeforeWindow = [Cell]
type Window = [Cell]
type AfterWindow = [Cell]
type Line = (BeforeWindow, Window, AfterWindow)
data TOKEN = RULE | LINES | START | WINDOW | MOVE | Value String
data Conf = Conf RuleNumber Start Lines WindowNumber Move
data EndConf = EndConf Rule Start Lines WindowNumber Move
newtype RuleNumber = RuleNumber (Maybe Int)
newtype Start = Start Int
newtype Lines = Lines (Maybe Int)
newtype WindowNumber = WindowNumber Int
newtype Move = Move Int
newtype LinesNum = LinesNum Int
