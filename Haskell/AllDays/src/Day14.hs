{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Control.Lens
import Data.List (sortOn, isPrefixOf)
import Data.Hashable
import GHC.Generics
import Debug.Trace
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

import Data.Char (digitToInt, isDigit)
import qualified Data.List as DL
import Data.List (foldl', foldr) 
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
data Instruction = Mask String | Memwrite Int Int deriving (Show)

type State = (BitMask, IntMap Value)
type Combiner = Char -> Char -> Char 

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



runInstructions :: Combiner -> [Instruction]  -> State
runInstructions c    = foldl (runInstruction c) ("", IntMap.empty) 

runInstruction :: Combiner -> State -> Instruction -> State
runInstruction c (mask, mp) i = case i of 
    (Mask m) -> (m, mp)
    (Memwrite a v) -> (mask, foldl (\ ac (a, v) -> writeMemory ac a v) mp  (applyMask2 c mask a v))  


writeMemory ::IntMap Value -> Addr -> Value ->   IntMap Value
writeMemory m a v   = IntMap.insert a v m

applyMask2 ::Combiner -> Mask -> Addr -> Value -> [(Addr, Value)]
applyMask2 c m a v = res  where
      zipdx = zipWith c m (padWith0 (length m) (toBinary a)) 
      res = case length $ filter (=='X') zipdx of 
          0 -> [(a, v')] where v' = toDec zipdx
          _ -> expandAddress zipdx  v 
              

expandAddress :: BinaryString  -> Value -> [(Addr, Value)]
expandAddress bs  v = res where
    bits = bitPerms $ length . filter (=='X') $ bs 
    bitsInxd = map (`zip` indicesX bs) bits   
    x = map (useListPair bs ) bitsInxd
    res = map (\b -> (toDec b, v))  x

useListPair :: BinaryString  -> [(Char, Int)] -> BinaryString
useListPair = foldl (\ac (c, i) -> (element i .~ c) ac) 
   
indicesX :: String -> [Int]
indicesX  = go 0 []  where
    go::Int->[Int]->String -> [Int]
    go _  ids [] =  reverse ids
    go ix ids (a:as) = if a == 'X' then go (ix+1) (ix:ids) as else go (ix+1) ids as


combine2::Char -> Char -> Char
combine2 mc' sc' 
    | mc' == 'X' = 'X'
    | mc' == '1' = '1'
    | otherwise = sc'

combine1::Char -> Char -> Char
combine1 mc' sc' 
    | mc' == 'X' = sc'
    | otherwise = mc'
    
applyMask :: Mask -> Value -> Value
applyMask m v  =  toDec . applyMask' m $ toBinary v  where 
    applyMask' m s = zipWith combine m (padWith0 (length m) s) where 
        combine::Char -> Char -> Char
        combine mc' sc' 
            | mc' == 'X' = sc'
            | otherwise = mc'
    


bitPerms :: Int -> [BinaryString]
bitPerms n =  map  (padWith '0' $ length . last $ bts) bts where
    bts =   [  toBinary i | i <-  [0.. 2^n - 1]]
   
toDec :: BinaryString -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

toBinary :: Int -> BinaryString
toBinary n = showIntAtBase 2 intToDigit n "" 

padWith0 = padWith '0' 
padWith ::Char -> Int -> String -> String
padWith c l  s 
    | length s == l = s
    | otherwise = padWith c l  (c:s)



day14Main :: IO () 
day14Main = do
    inp <- readFile "../data/Day14Part1.txt"
    let (_, mp) =    runInstructions combine1  (parseInstructions inp)
    let tot = IntMap.foldr (+) 0 mp
    print tot
    let (_, mp2) =    runInstructions combine2  (parseInstructions inp)
    let tot2 = IntMap.foldr (+) 0 mp2
    print tot2