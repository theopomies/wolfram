module Wolfram where

import Parser
import Generation
import Libs
import Error
import Control.Exception

displayLine :: Line -> IO ()
displayLine (_, window, _) = putStrLn window

display :: (EndConf, [Line]) -> IO ()
display ((EndConf _ (Start start) (Lines Nothing)       _ _), lines) = mapM_ displayLine $ drop start lines
display ((EndConf _ (Start start) (Lines (Just lineNo)) _ _), lines) = mapM_ displayLine $ take lineNo $ drop start lines

wolfram :: [String] -> IO ()
wolfram args = display $ getLines $ argsToInitialSetup args

argsToInitialSetup :: [String] -> (EndConf, Line)
argsToInitialSetup = firstLine . argsToEndConf