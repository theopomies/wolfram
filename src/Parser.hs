module Parser where

import Text.Read (readMaybe)
import Error
import Control.Exception
import Libs
import Generation

defaultConf :: Conf
defaultConf = Conf (RuleNumber Nothing) (Start 0) (Lines Nothing) (WindowNumber 80) (Move 0)

tokenize :: [String] -> [TOKEN]
tokenize []               = []
tokenize ("--rule" : xs)   = RULE : tokenize xs
tokenize ("--start" : xs)  = START : tokenize xs
tokenize ("--lines" : xs)  = LINES : tokenize xs
tokenize ("--window" : xs) = WINDOW : tokenize xs
tokenize ("--move" : xs)   = MOVE : tokenize xs
tokenize ("-h" : xs)      = throw UsageException
tokenize (x : xs)         = Value x : tokenize xs

parse :: [TOKEN] -> Conf -> Conf
parse []                      c = c
parse (RULE : Value v : xs)   c = parse xs $ setRule c $ readMaybe v
parse (START : Value v : xs)  c = parse xs $ setStart c $ readMaybe v
parse (LINES : Value v : xs)  c = parse xs $ setLines c $ readMaybe v
parse (WINDOW : Value v : xs) c = parse xs $ setWindow c $ readMaybe v
parse (MOVE : Value v : xs)   c = parse xs $ setMove c $ readMaybe v
parse _                       _ = throw ArgException

setRule :: Conf -> Maybe Int -> Conf
setRule (Conf _ start lines window move) (Just 30)  = Conf (RuleNumber (Just 30)) start lines window move
setRule (Conf _ start lines window move) (Just 90)  = Conf (RuleNumber (Just 90)) start lines window move
setRule (Conf _ start lines window move) (Just 110) = Conf (RuleNumber (Just 110)) start lines window move
setRule _                                _          = throw ArgException

setStart :: Conf -> Maybe Int -> Conf
setStart (Conf rule _ lines window move) (Just start)
    | start < 0 = throw ArgException
    | otherwise = Conf rule (Start start) lines window move
setStart _                               Nothing      = throw ArgException

setLines :: Conf -> Maybe Int -> Conf
setLines (Conf rule start _ window move) lines@(Just lineNo)
    | lineNo < 0 = throw ArgException
    | otherwise = Conf rule start (Lines lines) window move
setLines _                               Nothing      = throw ArgException

setWindow :: Conf -> Maybe Int -> Conf
setWindow (Conf rule start lines _ move) (Just window)
    | window < 0                                      = throw ArgException
    | otherwise                                       = Conf rule start lines (WindowNumber window) move
setWindow _                              Nothing      = throw ArgException

setMove :: Conf -> Maybe Int -> Conf
setMove (Conf rule start lines window _) (Just move) = Conf rule start lines window (Move move)
setMove _                                Nothing     = throw ArgException

getEndConf :: Conf -> EndConf
getEndConf (Conf (RuleNumber (Just 30))  start lines window move) = EndConf rule30  start lines window move
getEndConf (Conf (RuleNumber (Just 90))  start lines window move) = EndConf rule90  start lines window move
getEndConf (Conf (RuleNumber (Just 110)) start lines window move) = EndConf rule110 start lines window move
getEndConf _                                                      = throw ArgException

argsToEndConf :: [String] -> EndConf
argsToEndConf args = getEndConf $ parse (tokenize args) defaultConf