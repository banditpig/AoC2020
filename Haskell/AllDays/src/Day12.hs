{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -fwarn-typed-holes #-}
module Day12 where 
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map, keys)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Data.Hashable
import GHC.Generics
import Debug.Trace

type State = (Location, Facing)
type Location = (Int, Int)
data Facing = North | East | South | West deriving (Show)
data Action = N Int | S Int  | E Int  | W Int  | L Int  | R Int  | F Int deriving (Show)
type Actions = [Action]


-- Action N means to move north by the given value.
-- Action S means to move south by the given value.
-- Action E means to move east by the given value.
-- Action W means to move west by the given value.
-- Action L means to turn left the given number of degrees.
-- Action R means to turn right the given number of degrees.
-- Action F means to move forward by the given value in the direction the ship is currently facing.

parseActions :: [String] -> Actions
parseActions ls = actions where
    
        actions = map parse ls where 
        parse :: String -> Action
        parse bs = res where 
                n = read (tail bs):: Int
                res = case head bs of 
                    'N' -> N n
                    'S' -> S n
                    'E' -> E n
                    'W' -> W n
                    'L' -> L n
                    'R' -> R n
                    'F' -> F n

runActions :: State -> Actions -> State
runActions s as = foldl runAction s as

runAction :: State -> Action -> State
runAction st@((x, y), f) a = 
    case a of 
      N n  -> ((x, y - n), f)
      S n ->  ((x, y + n), f)
      E n ->  ((x + n, y), f)
      W n ->  ((x - n, y), f)
      L n ->  rot (L n) st
      R n ->  rot (R n) st
      F n ->  move st n where
          move :: State -> Int -> State
          move st@((x, y), f) n = 
              case f of 
                  North -> runAction st (N n)
                  East  -> runAction st (E n)
                  South -> runAction st (S n)
                  West  -> runAction st (W n)


toNum :: Facing -> Int
toNum  = \case
            North -> 0
            East  -> 1
            South -> 2
            West  -> 3
fromNum :: Int -> Facing
fromNum = \case
            0 -> North
            1 -> East
            2 -> South
            3 -> West

rot :: Action -> State -> State
rot (L n) ((x,y), f) = ((x, y), fromNum f') where
    f' = ((toNum f)  - (n `div` 90)) `mod` 4 

rot (R n) ((x,y), f) = ((x, y), fromNum f') where
    f' = ((toNum f)  + (n `div` 90)) `mod` 4 


part1 :: Actions -> Int
part1 as = dist where
    finalState@((x, y), face) = runActions ((0, 0), East) as
    dist = (abs x) + (abs y)

day12Main :: IO () 
day12Main = do
    actions <- parseActions . lines <$> readFile "../data/Day12Part1.txt"
    print (part1 actions)