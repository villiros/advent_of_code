module P10 where
import Common (ProbResult(PRNum), listSplit, observe, flatten)
import Data.Maybe (fromJust, isJust)
import Data.List (find, singleton, intersperse)
import Data.Tuple (swap)
import GHC.IO (unsafePerformIO)

-- Set value in list xs at index to v
lSetAt xs index v =
    let (left, _:right) = splitAt index xs
    in left ++ (v:right)

data Pos = Pos {row :: Int, col :: Int}
    deriving (Eq, Ord, Show)

data Matrix a = Matrix {
    matrix :: [[a]],
    nrows :: Int,
    ncols :: Int
} deriving (Show)

mkMatrix nrows ncols initElem =
    Matrix {matrix = [[initElem | _ <- [1..ncols]] | _ <- [1..nrows]],
            nrows = nrows,
            ncols = ncols}

setAt :: Matrix a -> Pos -> a -> Matrix a
setAt mat@(Matrix {matrix = mms}) pos v =
    mat {matrix = lSetAt mms (row pos)
            (lSetAt (mms !! row pos) (col pos) v)}

-- pos `isIn` matrix: is pos position inside the matrix's bounds?
(Pos pr pc) `isIn` (Matrix {nrows = nr, ncols = nc}) =
    pr >= 0 && pr < nr && pc >= 0 && pc < nc

-- Adds two positions (second position treated as a vector)
(+++) :: Pos -> Pos -> Pos
(Pos lr lc) +++ (Pos rr rc) = Pos (lr + rr) (lc + rc)

-- Grab a value from matrix.
(!??) :: Matrix a -> Pos -> Maybe a
mat@(Matrix {matrix = m}) !?? pos@(Pos r c)
    | pos `isIn` mat = Just $ m !! r !! c
    | otherwise = Nothing

-- List of coordinates adjacent to position p
adjacents p = [p +++ Pos ir ic | ir <- [-1..1], ic <- [-1..1], ir /= 0 || ic /= 0]

-- All positions in the matrix
allPos mat = [Pos r c | r <- [0..((nrows mat) - 1)], c <- [0..((ncols mat) - 1)]]

data Cell =
    PNS | PEW | PNE | PNW | PSW | PSE |
    GND | STR
    deriving (Show, Eq)

cellNameMap =
    [('|', PNS), ('-', PEW), ('L', PNE), ('J', PNW),
     ('7', PSW), ('F', PSE), ('.', GND), ('S', STR)]

instance Read Cell where
    readsPrec _ [] = []
    readsPrec _ (x:xs) =
        case lookup x cellNameMap of
            Just c -> [(c, xs)]
            Nothing -> []

-- Vectors defined by pipe bend types
cellMoves (Just PNS) = (Pos (-1) 0, Pos 1 0)
cellMoves (Just PEW) = (Pos 0 (-1), Pos 0 1)
cellMoves (Just PNE) = (Pos (-1) 0, Pos 0 1)
cellMoves (Just PNW) = (Pos (-1) 0, Pos 0 (-1))
cellMoves (Just PSW) = (Pos 0 (-1), Pos 1 0)
cellMoves (Just PSE) = (Pos 0 1, Pos 1 0)
cellMoves _ = (Pos 0 0, Pos 0 0)

matToString (Matrix {matrix = mat}) =
    flatten $ intersperse "\n" $ map (map cellToChar) mat
    where
        cellToChar e =
            fromJust $ lookup e $ map swap cellNameMap

boolMatToString (Matrix {matrix = mat}) =
    flatten $ intersperse "\n" $ map (map cellToChar) mat
    where
        cellToChar True = '.'
        cellToChar False = ' '


------------------------------------------------------------------------------------------------
-- Data reading
------------------------------------------------------------------------------------------------

readData :: [String] -> Matrix Cell
readData lns =
    Matrix {matrix = map (map (read . singleton)) lns,
            nrows = length lns,
            ncols = length (head lns)}

------------------------------------------------------------------------------------------------
-- Part A
------------------------------------------------------------------------------------------------

findStart :: Matrix Cell  -> Pos
findStart mat =
    fromJust $ find ((== STR) . fromJust . (mat !??)) $ allPos mat

-- Returns Just m if we can move into toPos along a pipe
-- Returns the next move out of toPos we can take
canMoveTo :: Matrix Cell -> Pos -> Pos -> Maybe Pos
canMoveTo mat fromPos toPos =
    case cellMoves $ mat !?? toPos of
        (m1, m2) | toPos +++ m1 == fromPos ->
            Just m2
        (m1, m2) | toPos +++ m2 == fromPos ->
            Just m1
        _ -> Nothing

followPath :: Matrix Cell -> Pos -> [Pos] -> [Pos]
followPath mat pos pathSoFar@(prevPos:_) =
    let (Just nextMove) = canMoveTo mat prevPos pos
        nextPos = pos +++ nextMove
        nextCell = mat !?? nextPos
    in
        case nextCell of
            Just STR -> pos:pathSoFar -- looped to the start
            Just GND -> []
            Nothing -> []
            -- Next cell is a pipe, and it allows moving into it along nextMove
            -- and we haven't visited it yet (didn't loop on itself)
            _ | (isJust $ canMoveTo mat pos nextPos) && (not $ elem nextPos pathSoFar) ->
                followPath mat nextPos (pos:pathSoFar)
            _ -> []

findLoop :: Matrix Cell -> [Pos]
findLoop mat =
    head $ dropWhile null $
        map (\x ->followPath mat x [startPos]) $
            filter (isJust . canMoveTo mat startPos) $
                adjacents startPos
    where
        startPos = findStart mat

solveA indata =
    let mat = readData indata
    in
        PRNum $ ((`div` 2) . length) $ findLoop mat


------------------------------------------------------------------------------------------------
-- Part B
------------------------------------------------------------------------------------------------

-- Assuming an 'S' is at pos, and next and prev pos are given, figure out
-- the correct pipe cell that should be at S
resolveStartPipe pos nextPos prevPos =
    head $ dropWhile isNotMatch [PNS, PEW, PNE, PNW, PSW, PSE]
    where
        isNotMatch2 (m1, m2)
            | (pos +++ m1) == nextPos && (pos +++ m2) == prevPos = False
            | (pos +++ m2) == nextPos && (pos +++ m1) == prevPos = False
            | (pos +++ m1) == prevPos && (pos +++ m2) == nextPos = False
            | (pos +++ m2) == prevPos && (pos +++ m1) == nextPos = False
            | otherwise = True
        isNotMatch pipe = isNotMatch2 $ cellMoves $ Just pipe

-- Given a pipe cell at a position, turn it into
-- a 3x3 mask, where True are where the pipe should be
-- Mask is returned as a list of absolute positions where pipe should be
upsample :: Pos -> Maybe Cell -> [Pos]
upsample (Pos pr pc) (Just c) =
    map (\[a, b] -> (Pos (pr * 3) (pc * 3)) +++ Pos a b) $ fromJust $ lookup c cellToNinerMap
    where
        cellToNinerMap =
            [(PNS, [[0, 1], [1, 1], [2, 1]]),
            (PEW, [[1, 0], [1, 1], [1, 2]]),
            (PNE, [[0, 1], [1, 1], [1, 2]]),
            (PNW, [[0, 1], [1, 1], [1, 0]]),
            (PSW, [[1, 0], [1, 1], [2, 1]]),
            (PSE, [[1, 2], [1, 1], [2, 1]])]

-- Very simple and slow flood fill from pos through elements of visited that are False
floodFill :: Matrix Bool -> Pos -> Matrix Bool
floodFill visited pos =
    foldl (\v2 p2 -> floodFill (setAt v2 p2 True) p2) visited nextSteps
    where
        nextSteps =
            filter (not . fromJust . (visited !??)) $ filter (isJust . (visited !??)) $ adjacents pos

-- Given a matrix that was upsampled by 3x and a position in
-- the original (non-upsampled) matrix, return True if there
-- are any True elements in the 3x3 block represented by pos
downsample (Matrix {matrix = mat}) pos =
    or [or $ take 3 $ drop (3 * col pos) (mat !! ((3 * row pos) + i)) | i <- [0..2]]

solveB inData =
    let mat = readData inData
        -- findLoop returns starting location at the end; make it 1st to simplify the following steps
        pipeLoop = reverse $ findLoop mat
        -- Now construct a visited matrix, which is 3x the size (each cell is 3x3)
        -- And draw the found pipe loop in it. Because of the 3x magnification, this
        -- will ensure that there's a border around the loop, and that there are
        -- spaces between vertical pipes, etc.
        -- Then we can flood-fill that as normal and count the number of 3x3 unvisited blocks
        visited0 = mkMatrix (3 * nrows mat) (3 * ncols mat) False
        -- Fill in visited with everything in pipeLoop except for the start
        visited1 =
            foldl (\vm p -> setAt vm p True) visited0 $
                flatten $
                map (\p -> upsample p (mat !?? p)) $
                tail pipeLoop
        -- And fill in the start block
        cellAtStart = resolveStartPipe (head pipeLoop) (pipeLoop !! 1) (last pipeLoop)
        visited2 =
            foldl (\vm p -> setAt vm p True) visited1 $
                upsample (head pipeLoop) (Just cellAtStart)
        -- Flood fill unvisited position
        visited3 = floodFill visited2 (Pos 0 0)
        -- And count original positions that are fully unvisited in visited 3
        result = length $ filter (not . downsample visited3) $ allPos mat
    in
        -- unsafePerformIO $ do
        --     putStrLn (matToString mat)
        --     putStrLn ""
        --     putStrLn (boolMatToString visited3)
        --     return $
                PRNum result
