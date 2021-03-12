module Main where

import Control.Exception
import System.Environment
import Wolfram
import Error

main :: IO ()
main = catch (getArgs >>= wolfram) exceptionHandler

