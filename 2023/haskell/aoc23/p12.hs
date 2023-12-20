module P12 where
import Common (ProbResult(PRNum), listSplit, observe, flatten)
import Data.List (intersperse)
import qualified Data.Map as M

-- Regex mat for exactly or at least the given number of chars
data Term =
    Exactly {symb :: Char, numb :: Int} |
    AtLeast {symb :: Char, numb :: Int}
    deriving (Show, Eq, Ord)

data MemoRet p r rf =
    MReturn r | MRecurse p
    | MReduce2 p p rf
    deriving (Show)

-- Hand-rolled memoisation
-- Call (fn p) in a memoize context.
-- fn should return MReturn (if it has a result),
-- or a MRecurse p (if (fn p) should be called)
-- or a MReduce p1 p2 rfun (if (rfun (fn p1) (fn p2)) is needed)
memoise :: Ord t => (t -> MemoRet t b (b -> b -> b)) -> t -> b
memoise fn p =
    snd $ memoiseInner M.empty p
    where
        memoiseInner memo p
            | M.member p memo = (memo, memo M.! p)
            | otherwise =
                case fn p of
                    MReturn r ->
                        (M.insert p r memo, r)
                    MRecurse p2 ->
                        memoiseInner memo p2
                    MReduce2 p2 p3 rf ->
                        let (memo2, r2) = memoiseInner memo p2
                            (memo3, r3) = memoiseInner memo2 p3
                            r = rf r2 r3
                        in
                            (M.insert p r memo3, r)

------------------------------------------------------------------------------------------------
-- Data reading
------------------------------------------------------------------------------------------------

readData :: Int -> [String] -> [(String, [Term])]
readData numDups lns =
    map readLine lns
    where
        readTerms lens =
            (AtLeast '.' 0) : (intersperse (AtLeast '.' 1) (map (Exactly '#') lens) ++ [AtLeast '.' 0])
        readLine ln =
            let [inputString, termLens] = listSplit (== ' ') ln
            in (flatten $ intersperse "?" $ replicate numDups inputString,
                readTerms $ map read $ listSplit (== ',') (flatten $ intersperse "," $ replicate numDups termLens))

------------------------------------------------------------------------------------------------
-- Part A
------------------------------------------------------------------------------------------------

countMatches (('?':ss), ts)                 = MReduce2 (('.':ss), ts) (('#':ss), ts) (+)
countMatches ((s:ss), (t@(Exactly {symb = tsymb, numb = numb}):ts))
    | s == tsymb && numb == 1               = MRecurse (ss, ts)
    | s == tsymb                            = MRecurse (ss, (t {numb = numb - 1} : ts))
    | otherwise                             = MReturn 0
countMatches ((s:ss), (t@(AtLeast {symb = tsymb, numb = numb}):ts))
    | s == tsymb                            = MRecurse (ss, (t { numb = max 0 (numb - 1)} : ts))
    | numb == 0                             = MRecurse ((s:ss), ts)
    | otherwise                             = MReturn 0
countMatches ((_:_), [])                    = MReturn 0
countMatches ([], (Exactly {} : _))         = MReturn 0
countMatches ([], (AtLeast {numb = 0}:ts))  = MRecurse ([], ts)
countMatches ([], [])                       = MReturn 1
countMatches (_, _)                         = MReturn 0


solveA lns =
    let dta = readData 1 lns
    in
        PRNum $ sum $ map (memoise countMatches) dta

------------------------------------------------------------------------------------------------
-- Part B
------------------------------------------------------------------------------------------------

solveB lns =
    let dta = readData 5 lns
    in
        PRNum $ sum $ map (memoise countMatches) dta
