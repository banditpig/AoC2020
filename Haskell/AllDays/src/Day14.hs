{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -fwarn-typed-holes #-}
module Day14 where 
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map, keys)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Data.List.Split ( splitOn )
import Data.List.Utils 
import Data.List
import Data.List (sortOn, isPrefixOf)
import Data.Hashable
import GHC.Generics
import Debug.Trace
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Text.Regex

type BinaryString = String
type BitMask = String

type Addr = Int
type Value = Int

type Mask = String
data Instruction = Mask String | Memwrite Int Int

type State = (BitMask, IntMap Value)
  

parseInstructions :: String -> [Instruction]
parseInstructions s = res where
    
    res = map parse $ lines s where 
        parse :: String -> Instruction
        parse s = x where 
         [a, _, c] = words s
         x = if  "mask" `isPrefixOf` a
             then
                 Mask c 
             else
                 Memwrite (read $ filter isDigit a) (read c :: Int)



runInstructions :: [Instruction]  -> State
runInstructions   = foldl runInstruction ("", IntMap.empty) 

runInstruction :: State -> Instruction -> State
runInstruction (mask, mp) i = case i of 
    (Mask m) -> (m, mp)
    (Memwrite a v) -> (mask, IntMap.insert a v' mp) where 
        v' = applyMask mask v

applyMask :: Mask -> Value -> Value
applyMask m v  =  toDec . applyMask' m $ toBinary v  where 
    applyMask' m s = zipWith combine m (padWith0 (length m) s) where 
        combine::Char -> Char -> Char
        combine mc' sc' 
            | mc' == 'X' = sc'
            | otherwise = mc'
    



toDec :: BinaryString -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

toBinary n = showIntAtBase 2 intToDigit n "" 

padWith ::Char -> Int -> String -> String
padWith c l  s 
    | length s == l = s
    | otherwise = padWith c l  (c:s)

padWith0 = padWith '0' 

day14Main :: IO () 
day14Main = do
    inp <- readFile "../data/Day14Part1.txt"
    let (_, mp) =    runInstructions . parseInstructions $ inp
    let tot = IntMap.foldr (+) 0 mp
    
    print tot