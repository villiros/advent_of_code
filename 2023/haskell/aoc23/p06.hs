module P06 where
import Common (ProbResult(PRNum), observe, listSplit)
import Data.List (transpose)
import Data.Char (isDigit)

data Race = Race {time :: Int, distance :: Int}
    deriving (Show)

------------------------------------------------------------------------------------------------
-- Data reading
------------------------------------------------------------------------------------------------
readData [a, b] =
    zipWith Race (map read
                    (tail (listSplit (== ' ') a)))
                (map read (tail (listSplit (== ' ') b)))

readDataB [a, b] =
    Race (read (filter isDigit a)) (read (filter isDigit b))

------------------------------------------------------------------------------------------------
-- Part A
------------------------------------------------------------------------------------------------

emulate :: Int -> Int -> Int
emulate maxTime buttonTime =
    let speed = buttonTime
        remTime = max 0 (maxTime - buttonTime)
    in
        remTime * speed

countWaysA (Race {time = time, distance = distance}) =
    length $ filter (> distance) $ map (emulate time) [0..(time-1)]

solveA dta =
    PRNum $ product $ map countWaysA (readData dta)

------------------------------------------------------------------------------------------------
-- Part B
------------------------------------------------------------------------------------------------
solveB dta =
    PRNum $ countWaysA $ readDataB dta
