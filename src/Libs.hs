module Libs where

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

type Rule = Cell -> Cell -> Cell -> Cell
newtype Cell = Cell Char deriving Show
newtype BeforeWindow = BeforeWindow [Cell]
newtype Window = Window [Cell]
newtype AfterWindow = AfterWindow [Cell]
newtype Line = Line (BeforeWindow, Window, AfterWindow)
data TOKEN = RULE | LINES | START | WINDOW | MOVE | Value String
data Conf = Conf RuleNumber Start Lines WindowNumber Move
data EndConf = EndConf Rule Start Lines WindowNumber Move
newtype RuleNumber = RuleNumber (Maybe Int)
newtype Start = Start Int
newtype Lines = Lines (Maybe Int)
newtype WindowNumber = WindowNumber Int
newtype Move = Move Int
newtype LinesNum = LinesNum Int