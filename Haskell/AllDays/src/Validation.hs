{-# LANGUAGE QuasiQuotes                      #-}
module Validation where

import Text.RE.TDFA.String ( (?=~), re, matched )
import Data.Char ( isNumber )

newtype Error = Error String deriving (Eq, Show)

type Validator = String -> Either Error String
toInt :: String -> Int
toInt s = read s :: Int

alwaysRight :: String  ->  Either Error String
alwaysRight _ = Right "Always OK"

numberCheck :: Int -> Int -> String -> Either Error String
numberCheck lo hi s 
    | n >= lo && n <= hi = Right s
    | otherwise = Left (Error $  "Int out of range. " ++ show lo ++ " " ++ show hi ++ " " ++ s ) where n = toInt s

birthCheck :: String -> Either Error String
birthCheck  = numberCheck 1920 2002 

issueCheck :: String -> Either Error String
issueCheck = numberCheck 2010 2020 

expireCheck :: String -> Either Error String
expireCheck   = numberCheck 2020 2030

eyeCheck :: String -> Either Error String
eyeCheck e 
    |  e `elem`   ["amb","blu","brn", "gry", "grn", "hzl","oth"] = Right e
    |  otherwise = Left (Error $ "Bad eye. " ++ e)



hairCheck :: String -> Either Error String
hairCheck h 
    | matched $ h ?=~ [re|#[0-9,a-f]{6}$|] = Right h
    | otherwise  = Left  (Error $ "Bad hair day. " ++ h)


pidCheck :: String -> Either Error String
pidCheck p 
    | matched $ p ?=~ [re|^[0-9]{9}$|] = Right p
    | otherwise  = Left  (Error $ "Bad pid. " ++ p)


heightCheck :: String -> Either Error String 
heightCheck h = res where
    nmbr = takeWhile isNumber h
    units = dropWhile isNumber h
    res = case units of
           "cm" -> numberCheck 150 193 nmbr
           "in" -> numberCheck 59 76 nmbr
           _ ->    Left (Error $ "Bad height. " ++ h)


validate :: [Validator ] -> String -> Either Error String
validate [v] s    = v s
validate (v:vs) s = v s >>= validate vs

