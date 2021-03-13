module Parser (parse, Conf (..), WindowStart (..), WindowLength (..), WindowWidth (..), WindowMove (..)) where

import Text.Read (readMaybe)
import Error
import Control.Exception
import Rules (Rule, RuleNumber (..), getRule)
import Lexer (TOKEN (..))

data Conf = Conf Rule WindowStart WindowLength WindowWidth WindowMove
newtype WindowStart  = WindowStart Int
newtype WindowLength = WindowLength (Maybe Int)
newtype WindowWidth  = WindowWidth Int
newtype WindowMove   = WindowMove Int

data WorkingConf = WorkingConf (Maybe RuleNumber) WindowStart WindowLength WindowWidth WindowMove

parse :: [TOKEN] -> Conf
parse = flip parse' defaultConf

toConf :: WorkingConf -> Conf
toConf (WorkingConf rule start ls window move) = Conf (getRule $ validateMaybe rule) start ls window move

defaultConf :: WorkingConf
defaultConf = WorkingConf Nothing (WindowStart 0) (WindowLength Nothing) (WindowWidth 80) (WindowMove 0)

parse' :: [TOKEN] -> WorkingConf -> Conf
parse' []                      c = toConf c
parse' (RULE : Value v : xs)   c = parse' xs $ setRule c $ readRule v
parse' (START : Value v : xs)  c = parse' xs $ setWindowStart c $ readWindowStart v
parse' (LINES : Value v : xs)  c = parse' xs $ setWindowLength c $ readLine v
parse' (WINDOW : Value v : xs) c = parse' xs $ setWindow c $ readWindow v
parse' (MOVE : Value v : xs)   c = parse' xs $ setWindowMove c $ readWindowMove v
parse' _                       _ = throw ArgException

readLine :: String -> Int
readLine = isPositive . validateMaybe . readMaybe

readWindowStart :: String -> Int
readWindowStart = isPositive . validateMaybe . readMaybe

readRule :: String -> Int
readRule = isValidRule . validateMaybe . readMaybe

readWindow :: String -> Int
readWindow = isPositive . validateMaybe . readMaybe

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

setWindow :: WorkingConf -> Int -> WorkingConf
setWindow (WorkingConf rule start ls _ move) window = WorkingConf rule start ls (WindowWidth window) move

setWindowMove :: WorkingConf -> Int -> WorkingConf
setWindowMove (WorkingConf rule start ls window _) move = WorkingConf rule start ls window $ WindowMove move