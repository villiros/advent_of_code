module P11 where
import Common (ProbResult(PRNum), observe, flatten)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (mapAccumL, tails)

data Pos = Pos {row :: Int, col :: Int}
    deriving (Eq, Ord, Show)

distance (Pos r1 c1) (Pos r2 c2) =
    (abs (r2 - r1)) + (abs (c2 - c1))

------------------------------------------------------------------------------------------------
-- Data reading
------------------------------------------------------------------------------------------------

readData lns =
    foldl (\acc (i, ln) -> readLine (Pos i) ln ++ acc) [] $ zip [0..] lns
    where
        readLine ppos ln =
            [ppos i | (i, c) <- zip [0..] ln, c == '#']

------------------------------------------------------------------------------------------------
-- Part A
------------------------------------------------------------------------------------------------

doExpands :: Int -> (Pos -> Int) -> (Pos -> Int -> Pos) -> [Pos] -> [Pos]
doExpands expandBy getter setter dta =
    let maxInd = maximum $ map getter dta
        freeInds = foldr (S.delete . getter) (S.fromList [0..maxInd]) dta
        incBy = M.fromList $ snd $ mapAccumL (\sofar i -> (sofar + (if S.member i freeInds then expandBy else 0), (i, i + sofar))) 0 [0..maxInd]
    in
        [setter x (incBy M.! getter x) | x <- dta]

doExpandsA = doExpands 1

allPairs lst =
    flatten $ map (zip lst) $ (tail . tails) lst

genSolver expander lns =
    let dta = readData lns
        dta2 = expander col (\x v -> x{col = v}) $ expander row (\x v -> x{row = v}) dta
    in
        --observe (dta, dta2) $
        PRNum $ sum $ map (uncurry distance) (allPairs dta2)


solveA = genSolver doExpandsA

------------------------------------------------------------------------------------------------
-- Part B
------------------------------------------------------------------------------------------------

doExpandsB = doExpands 999999

solveB = genSolver doExpandsB

