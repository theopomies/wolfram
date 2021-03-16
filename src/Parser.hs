{-# LANGUAGE CPP #-}
module Parser
#ifndef TESTS
(parse, Conf (..), WindowStart (..), WindowLength (..), WindowWidth (..), WindowMove (..))
#endif
where

import Text.Read (readMaybe)
import Error ( MyException(ArgException) )
import Control.Exception ( throw )
import Rules (Rule, RuleNumber (..), getRule)
import Lexer (TOKEN (..))

data Conf = Conf Rule WindowStart WindowLength WindowWidth WindowMove
  deriving Eq
newtype WindowStart  = WindowStart Int
  deriving Eq
newtype WindowLength = WindowLength (Maybe Int)
  deriving Eq
newtype WindowWidth  = WindowWidth Int
  deriving Eq
newtype WindowMove   = WindowMove Int
  deriving Eq

data WorkingConf = WorkingConf (Maybe RuleNumber) WindowStart WindowLength WindowWidth WindowMove
  deriving Eq

parse :: [TOKEN] -> Conf
parse = flip parse' defaultConf

toConf :: WorkingConf -> Conf
toConf (WorkingConf rule start ls window move) = Conf (getRule $ validateMaybe rule) start ls window move

defaultRule :: Maybe RuleNumber
defaultRule = Nothing

defaultStart :: WindowStart
defaultStart = WindowStart 0

defaultLength :: WindowLength
defaultLength = WindowLength Nothing

defaultWidth :: WindowWidth
defaultWidth = WindowWidth 80

defaultMove :: WindowMove
defaultMove = WindowMove 0

defaultConf :: WorkingConf
defaultConf = WorkingConf Nothing defaultStart defaultLength defaultWidth defaultMove

parse' :: [TOKEN] -> WorkingConf -> Conf
parse' []                      c = toConf c
parse' (RULE : Value v : xs)   c = parse' xs $ setRule c $ readRule v
parse' (START : Value v : xs)  c = parse' xs $ setWindowStart c $ readWindowStart v
parse' (LINES : Value v : xs)  c = parse' xs $ setWindowLength c $ readWindowLength v
parse' (WINDOW : Value v : xs) c = parse' xs $ setWindowWidth c $ readWindowWidth v
parse' (MOVE : Value v : xs)   c = parse' xs $ setWindowMove c $ readWindowMove v
parse' _                       _ = throw ArgException

readWindowLength :: String -> Int
readWindowLength = isPositive . validateMaybe . readMaybe

readWindowStart :: String -> Int
readWindowStart = isPositive . validateMaybe . readMaybe

readRule :: String -> Int
readRule = isValidRule . validateMaybe . readMaybe

readWindowWidth :: String -> Int
readWindowWidth = isPositive . validateMaybe . readMaybe

readWindowMove :: String -> Int
readWindowMove = validateMaybe . readMaybe

validateMaybe :: Maybe a -> a
validateMaybe Nothing  = throw ArgException
validateMaybe (Just e) = e

isValidRule :: Int -> Int
isValidRule n
    | n < 0 || n > 255 = throw ArgException
    | otherwise        = n

isPositive :: (Ord a, Num a) => a -> a
isPositive n
    | n < 0     = throw ArgException
    | otherwise = n

setRule :: WorkingConf -> Int -> WorkingConf
setRule (WorkingConf _ start ls window move) number = WorkingConf (Just (RuleNumber number)) start ls window move

setWindowStart :: WorkingConf -> Int -> WorkingConf
setWindowStart (WorkingConf rule _ ls window move) start = WorkingConf rule (WindowStart start) ls window move

setWindowLength :: WorkingConf -> Int -> WorkingConf
setWindowLength (WorkingConf rule start _ window move) ls = WorkingConf rule start (WindowLength $ Just ls) window move

setWindowWidth :: WorkingConf -> Int -> WorkingConf
setWindowWidth (WorkingConf rule start ls _ move) window = WorkingConf rule start ls (WindowWidth window) move

setWindowMove :: WorkingConf -> Int -> WorkingConf
setWindowMove (WorkingConf rule start ls window _) move = WorkingConf rule start ls window $ WindowMove move