module Lexer (TOKEN (..), tokenize) where

import Error ( MyException(UsageException) )
import Control.Exception ( throw )

data TOKEN = RULE | LINES | START | WINDOW | MOVE | Value String

tokenize :: [String] -> [TOKEN]
tokenize []                = []
tokenize ("--rule" : xs)   = RULE : tokenize xs
tokenize ("--start" : xs)  = START : tokenize xs
tokenize ("--lines" : xs)  = LINES : tokenize xs
tokenize ("--window" : xs) = WINDOW : tokenize xs
tokenize ("--move" : xs)   = MOVE : tokenize xs
tokenize ("-h" : xs)       = throw UsageException
tokenize (x : xs)          = Value x : tokenize xs