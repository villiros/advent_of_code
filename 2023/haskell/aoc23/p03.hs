module P03 where
    
import System.IO
import Data.Maybe
import Data.List 
import Data.Char (digitToInt, isDigit)
import Control.Exception (assert)
import Debug.Trace (trace)
import Common (ProbResult(PRNum))

-- CNumber / CSymbol are inputs. CNothing is input '.'
-- Later the matrix will be parsed and horizontal runs of CNumber
-- are replaced with CParsedNumbe
-- Eg: 123. will be replaced with [CParsedNumber 123 _pos1, CParsedNumber 123 _pos1, CParsedNumber 123 _pos1]
data Cell =
    CNumber Char | CSymbol Char | CNothing 
    | CParsedNumber Int Pos -- <actual value> <start position>
    deriving (Eq, Ord, Show)

isCNumber (CNumber _) = True
isCNumber _ = False

fromCNumber (CNumber c) = c

isCSymbol (CSymbol _) = True
isCSymbol _ = False

isParsedNumber (CParsedNumber _ _) = True
isParsedNumber _ = False

data Matrix = Matrix {
    matrix :: [[Cell]],
    nrows :: Int,
    ncols :: Int
} deriving (Show)

data Pos = Pos {row :: Int, col :: Int}
    deriving (Eq, Ord, Show)

-- pos `isIn` matrix: is pos position inside the matrix's bounds?
(Pos pr pc) `isIn` (Matrix {nrows = nr, ncols = nc}) =
    pr >= 0 && pr < nr && pc >= 0 && pc < nc

-- Adds two positions (second position treated as a vector)
(+++) :: Pos -> Pos -> Pos
(Pos lr lc) +++ (Pos rr rc) = Pos (lr + rr) (lc + rc)

-- Grab a value from matrix. Returns CNothing if out of bounds.
(!??) :: Matrix -> Pos -> Cell
mat@(Matrix {matrix = m}) !?? pos@(Pos r c)
    | pos `isIn` mat = m !! r !! c
_ !?? _ = CNothing

-- List of coordinates adjacent to position p
adjacents p = [p +++ Pos ir ic | ir <- [-1..1], ic <- [-1..1], ir /= 0 || ic /= 0]

-- All positions in the matrix
allPos mat = [Pos r c | r <- [0..((nrows mat) - 1)], c <- [0..((ncols mat) - 1)]]

------------------------------------------------------------------------------------------------
--
-- Data reading
--
------------------------------------------------------------------------------------------------

toCell '.' = CNothing
toCell n | isDigit n = CNumber n
toCell c = CSymbol c

makeMatrix :: [String] -> Matrix
makeMatrix inRows =
    let rows = length inRows
        cols = length (head inRows)
        m = map (map toCell) inRows in
            assert ((all (\x -> (length x) == cols) m) && (length m) == rows) $
            Matrix {matrix = m, nrows = rows, ncols = cols}

readData lines =
    makeMatrix lines

------------------------------------------------------------------------------------------------
--
-- Part A
--
------------------------------------------------------------------------------------------------

parseNumsInRow rowNum colNum [] = []
parseNumsInRow rowNum colNum dta@(x:xs) =
    case takeWhile isCNumber dta of
        [] -> x : parseNumsInRow rowNum (colNum + 1) xs
        num ->
            let parsed = CParsedNumber (read $ map fromCNumber num) (Pos rowNum colNum)
                prefLen = length num 
                prefix = replicate prefLen parsed in
                    prefix ++ parseNumsInRow rowNum (colNum + prefLen) (drop prefLen dta)

-- Given a matrix, replace all runs of CNumber with CParsedNumber
parseNums mat =
    mat {matrix = [parseNumsInRow i 0 r | (i, r) <- zip [0..((nrows mat) - 1)] (matrix mat)]}

findPartNumbers = findPartNumbers2 . parseNums

findPartNumbers2 mat =
    nub $ map (mat !??) $ filter (\pos -> isParsedNumber (mat !?? pos) && isAdjacentSymbol pos) (allPos mat)
    where isAdjacentSymbol pos = any isCSymbol [mat !?? i | i <- adjacents pos]

solveA d =
    let nums = findPartNumbers (readData d) in
        PRNum $ sum $ map (\(CParsedNumber r _) -> r) nums

------------------------------------------------------------------------------------------------
--
-- Part B
--
------------------------------------------------------------------------------------------------

findGearNumbers = findGearNumbers2 . parseNums

findGearNumbers2 mat =
    let gearPos = filter ((CSymbol '*' == ) . (mat !??)) (allPos mat) in
        sum [case adjacentNums p of
                [CParsedNumber a _, CParsedNumber b _] -> a * b
                _ -> 0
            | p <- gearPos]
    where adjacentNums pos = nub [c | p <- adjacents pos, c <- [mat !?? p], isParsedNumber c]

solveB = PRNum . findGearNumbers . readData
