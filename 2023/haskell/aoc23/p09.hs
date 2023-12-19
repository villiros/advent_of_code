module P09 where
import Common (ProbResult(PRNum), listSplit)

------------------------------------------------------------------------------------------------
-- Data reading
------------------------------------------------------------------------------------------------

readData :: [String] -> [[Int]]
readData =
        map readLine
    where
        readLine = map read . listSplit (== ' ')

------------------------------------------------------------------------------------------------
-- Part A
------------------------------------------------------------------------------------------------

mkdiff [_] = []
mkdiff (a:b:bs) =
    (b-a):mkdiff (b:bs)

solveAone :: [Int] -> Int
solveAone nums =
    sum $ map last $ takeWhile (any (/= 0)) $ iterate mkdiff nums

solveA lns =
    PRNum $ sum $ map solveAone (readData lns)

------------------------------------------------------------------------------------------------
-- Part B
------------------------------------------------------------------------------------------------

solveBone nums =
    foldr1 (-) $ map head $ takeWhile (any (/= 0)) $ iterate mkdiff nums

solveB lns =
    PRNum $ sum $ map solveBone (readData lns)