module Main where

import Parser
import Error
import Generation
import Libs
import Control.Exception
import System.Environment

func :: Line -> IO ()
func (a, b, c) = putStrLn (take 10 a) >> putStrLn (take 10 b) >> putStrLn (take 10 c)

wolfram :: [String] -> IO ()
wolfram = func . wolf

wolf :: [String] -> Line
wolf = firstLine . argsToEndConf

main :: IO ()
main = catch (getArgs >>= wolfram) exceptionHandler

