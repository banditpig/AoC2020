{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -fwarn-typed-holes #-}
module Day13 where 
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map, keys)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Data.List.Split ( splitOn )
import Data.List (sortOn)
import Data.Hashable
import GHC.Generics
import Debug.Trace

 
type BusId = Int
type Estimate = Int
type LeaveTime = Int

procBusId :: Estimate -> BusId -> (LeaveTime, BusId)
procBusId e id = (leave, id) where 
    (d, r) = e `divMod` id
    leave = id - r 


procAllIds :: Estimate -> [BusId] -> LeaveTime
procAllIds e ids = t * id where
    (t, id) = head $ sortOn fst $ map (procBusId e) ids
    
readBusData :: String -> (Estimate, [BusId])
readBusData s =  (read target:: Int, ids' )  where 
    target:ids = lines s
    ids' = map (\v -> read v::Int) . filter (/= "x") . splitOn "," $ head ids

  
day13Main :: IO () 
day13Main = do
    inp <- readFile "../data/Day13Part1.txt"
    let (estmt, ids) = readBusData inp
    print (procAllIds estmt ids)