{-# LANGUAGE CPP #-}
module Display
#ifndef TESTS
(toScreen)
#endif
where

import Parser (Conf (..), WindowLength (..), WindowStart (..))
import Generation (Lines)

toScreen :: Conf -> Lines -> Lines
toScreen (Conf _ start (WindowLength Nothing)       _ _) ls = skipLines start ls
toScreen (Conf _ start (WindowLength (Just lineNo)) _ _) ls = take lineNo $ skipLines start ls

skipLines :: WindowStart -> Lines -> Lines
skipLines (WindowStart 0)     ls = ls
skipLines (WindowStart start) ls = drop start ls