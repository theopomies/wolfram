module Error where

import System.Exit ( exitSuccess, exitWith, ExitCode(ExitFailure) )
import Control.Exception ( Exception )
import System.Environment()

data MyException = ArgException
                 | UsageException
                 | ExhaustiveException
                 deriving Show
instance Exception MyException

printHelp :: IO ()
printHelp = mapM_  putStrLn ["Usage:",
                             "\t./wolfram -h : Displays this Usage section",
                             "\t./wolfram -rule n [-start n] [-lines n] [-window n] [-move n]",
                             "",
                             "Description:",
                             "\tThis project implements Wolfram’s elementary cellular automaton in the terminal.",
                             "",
                             "Flags:",
                             "\tMANDATORY",
                             "\t-rule: The rule for next generation. (30, 90 or 110)",
                             "",
                             "\tOPTIONNAL",
                             "\t–start: the generation numberat which to start the display. The default value is 0.",
                             "\t–lines: the number of lines to display. When ommited, the program never stops.",
                             "\t–window: the number of cells to display on each line (line width).",
                             "\t         If even, the central cell is displayed in the next cell on the right.",
                             "\t         The default value is 80.",
                             "\t–move: a translation to apply on the window.",
                             "\t       If negative, the window is translated to the left.",
                             "\t       If positive, it’s translated to the right."]

exceptionHandler :: MyException -> IO ()
exceptionHandler UsageException = printHelp >> exitSuccess
exceptionHandler _              = printHelp >> exitWith (ExitFailure 84)
