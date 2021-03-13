module Main where

import Control.Exception
import System.Environment
import Error
import Parser (parse)
import Lexer (tokenize)
import Generation (Window, getLines)
import Display (toScreen)

main :: IO ()
main = catch (getArgs >>= (mapM_ print . wolfram)) exceptionHandler

wolfram :: [String] -> [Window]
wolfram args = let conf = parse $ tokenize args in toScreen conf $ getLines conf