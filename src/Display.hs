module Display where

import Parser (Conf (..), WindowLength (..), WindowStart (..))
import Generation (Lines)

toScreen :: Conf -> Lines -> Lines
toScreen (Conf _ start (WindowLength Nothing)       _ _) lines = skipLines start lines
toScreen (Conf _ start (WindowLength (Just lineNo)) _ _) lines = take lineNo $ skipLines start lines

skipLines :: WindowStart -> Lines -> Lines
skipLines (WindowStart 0)     lines = lines
skipLines (WindowStart start) lines = drop start lines
