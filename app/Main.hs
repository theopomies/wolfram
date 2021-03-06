module Main where

import Control.Exception ( catch )
import System.Environment ( getArgs )
import Error ( exceptionHandler )
import Parser (parse, validateConf)
import Lexer (tokenize)
import Generation (Window, getLines)
import Display (toScreen)

main :: IO ()
main = catch (getArgs >>= (mapM_ print . wolfram)) exceptionHandler

wolfram :: [String] -> [Window]
wolfram args = let conf = validateConf $ parse $ tokenize args in toScreen conf $ getLines conf
