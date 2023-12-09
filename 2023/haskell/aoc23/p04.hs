module P04 where

import Data.List (map, intersect)
import qualified Data.Map as Map
import Common (ProbResult(PRNum))
import Debug.Trace (traceShow)

data Card =
    Card {cid :: Int,
          winning :: [Int],
          have :: [Int]}

------------------------------------------------------------------------------------------------
--
-- Data reading
--
------------------------------------------------------------------------------------------------

readData =
    map (readCard . words)
    where
        readCard ("Card":idStr:rest) =
            case break (== "|") rest of
                (winning, "|":have) ->
                    Card {cid = read (init idStr) :: Int,
                          winning = map read winning,
                          have = map read have}
------------------------------------------------------------------------------------------------
--
-- Part A
--
------------------------------------------------------------------------------------------------

numMatching (Card {winning = w, have = h}) = length $ w `intersect` h

solveA d =
    PRNum $ sum $ map (\x -> 2^(x-1)) $ filter (> 0) (map numMatching (readData d))

------------------------------------------------------------------------------------------------
--
-- Part B
--
------------------------------------------------------------------------------------------------

procCard :: Map.Map Int Int -> Card -> Map.Map Int Int
procCard counts c =
    let nm = numMatching c
        numToAdd = (counts Map.! (cid c))
    in
    if nm > 0 then
        foldr (Map.adjust (+ numToAdd)) counts [(cid c + 1)..(cid c + nm)]
    else
        counts

solveB d =
    let cards = readData d
        counts = Map.fromList $ map (\(Card {cid = cid}) -> (cid, 1)) cards
    in
        PRNum $ Map.foldr (+) 0 $ foldl procCard counts cards