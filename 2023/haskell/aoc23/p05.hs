module P05 where
import Common (ProbResult(PRNum), listSplit, observe)
import Control.Exception (assert)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map as M

-- start and end are inclusive
data ARange = ARange {start :: Int, end :: Int} deriving (Show, Eq)
data RangeMap = RangeMap {src :: ARange, dst :: ARange} deriving (Show)

-- String is the target type of this map
data AMap = AMap String [RangeMap] deriving (Show)
-- Maps index by type
data Almanac = Almanac {seeds :: [Int],
                        maps :: M.Map String AMap}
            deriving (Show)

isValid (ARange s e) = e >= s

-- Return a range common to both
rintersect :: ARange -> ARange -> [ARange]
rintersect (ARange ls le) (ARange rs re)
    = filter isValid [ARange (max ls rs) (min le re)]

-- Remove 2nd from 1st range
rdifference :: ARange -> ARange -> [ARange]
rdifference l@(ARange ls le) r@(ARange rs re)
    | rintersect l r == []  = [l]                           -- No overlap
    | rintersect l r == [l] = []                            -- rhs fully covers l
    | rintersect l r == [r] = [ARange ls (rs-1), ARange (re+1) le]  -- l fully covers rhs; make a hole 
    | ls < rs               = [ARange ls (rs - 1)]          -- [lll[rrr]rrr
    | ls < re               = [ARange (re + 1) le]          -- [rrr[llll]lll

-- Map a seet of ranges with a RangeMap (a single mapping)
-- Returns (<ranges mapped>, <unmapped>) where unmapped
-- ones haven't intersected with the src; other RangeMaps
-- should be attempted on them.
applyRangeMap :: [ARange] -> RangeMap -> ([ARange], [ARange])
applyRangeMap rngs rmap@(RangeMap {src = src, dst = dst}) =
    (foldl (++) [] $ map applyOneRange rngs,
     foldl (++) [] $ map (\x -> rdifference x src) rngs)
    where
        applyOneRange rng =
            observe (rng, rmap) $
            map (\(ARange s d) -> ARange ((start dst) + (s - (start src)))
                                         ((start dst) + (d - (start src))))
                (rintersect rng src)

------------------------------------------------------------------------------------------------
-- Data reading
------------------------------------------------------------------------------------------------

parseNums :: String -> [Int]
parseNums = map read . words

readMap (headStr:rangeStrs) =
    assert (" map:" `isSuffixOf` headStr) $
    let from:_:to:_head = listSplit (== '-') $ head (listSplit (== ' ') headStr)
        rangeMaps = map (mkRangeMap . parseNums) rangeStrs
    in
        (from, to, rangeMaps)
    where
        mkRangeMap [a, b, c] = RangeMap {dst = ARange a (a + c - 1), src = ARange b (b + c - 1)}


readData :: [[Char]] -> Almanac
readData (seedsStr:[]:mapsStr) =
    assert (isPrefixOf "seeds: " seedsStr) $
    let seeds = parseNums $ dropWhile (/= ' ') seedsStr
        ranges = map readMap (listSplit (== []) mapsStr)
        maps = foldl (\acc (f, t, rs) -> M.insert f (AMap t rs) acc) M.empty ranges
    in
        Almanac seeds maps


------------------------------------------------------------------------------------------------
-- Part A
------------------------------------------------------------------------------------------------

resolve :: Almanac -> String -> [ARange] -> [ARange]
resolve _ "location" rngs =
    observe ("location", rngs) $
    rngs
resolve alm@(Almanac{maps = m}) name rngs =
    observe (name, rngs) $
    let (Just (AMap next rangeMaps)) = M.lookup name m
        (mapped, unmapped) = foldl doApply ([], rngs) rangeMaps
    in
        resolve alm next (mapped ++ unmapped)
    where
        doApply (mapped, unmapped) rmap =
            let (m, um) = applyRangeMap unmapped rmap
            in (mapped ++ m, um)

solveA d =
    let alm = readData d
        locs = foldr1 (++) $ map (resolve alm "seed") $ map (\x -> [ARange x x]) $ seeds alm
    in
        PRNum $ minimum $ map start locs
        
------------------------------------------------------------------------------------------------
-- Part B
------------------------------------------------------------------------------------------------

solveB d =
    let alm = readData d
        locs = foldr (++) [] $ map (resolve alm "seed") $ mkseeds $ seeds alm
    in
        PRNum $ minimum $ map start locs
    where
        mkseeds (a:b:xs) = [ARange a (a+b-1)] : mkseeds xs
        mkseeds [] = []
    