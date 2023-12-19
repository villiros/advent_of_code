module P08 where
import Common (ProbResult(PRNum), observe, observeIf)
import qualified Data.Map as M
import Data.Maybe (fromJust)

data InfSeq a =
    InfSeq {cur :: [a], full :: [a], produced :: Int}
    deriving (Show)

pop f@(InfSeq{cur = [], full = a:as, produced = produced}) =
    (a, f {cur = as, produced = produced + 1})
pop f@(InfSeq{cur = a:as, produced = produced}) =
    (a, f {cur = as, produced = produced + 1})

type NodeName = String
data Node =
    Node {left :: NodeName, right :: NodeName}
    deriving (Show)

data AInput =
    AInput {inst :: InfSeq Char,
            network :: M.Map NodeName Node}
    deriving (Show)

popInst f@(AInput {inst = inst}) =
    let (r, inst2) = pop inst
    in (r, f {inst = inst2})

------------------------------------------------------------------------------------------------
-- Data reading
------------------------------------------------------------------------------------------------
readData :: [String] -> AInput
readData (inst:_:nwlines) =
    AInput {inst = InfSeq {cur = inst, full = inst, produced = 0},
            network = foldl readEnt M.empty nwlines}
    where
        readEnt m [nna, nnb, nnc, ' ', '=', ' ', '(', la, lb, lc, ',', ' ', ra, rb, rc, ')'] =
            M.insert [nna, nnb, nnc] (Node {left = [la, lb, lc], right = [ra, rb, rc]}) m

------------------------------------------------------------------------------------------------
-- Part A
------------------------------------------------------------------------------------------------

atraverse inp@(AInput {inst = inst, network = network}) cur =
    let (op, inp2) = case popInst inp of
            ('L', inp2) -> (left, inp2)
            ('R', inp2) -> (right, inp2)
    in
        cur : atraverse inp2 (op $ fromJust $ M.lookup cur network)

solveA inlines =
    PRNum $ length $ takeWhile (/= "ZZZ") $ atraverse (readData inlines) "AAA"

------------------------------------------------------------------------------------------------
-- Part B
------------------------------------------------------------------------------------------------

solveB inlines =
    let ainput = readData inlines
        stepsToZ = map (length . takeWhile ((/= 'Z') . last) . atraverse ainput) $ filter ((== 'A') . last) $ M.keys (network ainput)
        numInstructions = observe stepsToZ $ (length . cur . inst) ainput
    in
        -- Our given input is setup such that all starting positions
        -- get to a finish at a point where instructions are fully consumed,
        -- and that's the period. Also, the number of steps in each period is
        -- of the form <prime> * numInstructions, and they are different
        -- prime for each start.
        -- Though the example doesn't have numInstructions in each term...
        if numInstructions > 2 then
            PRNum $ numInstructions * product (map (`div` numInstructions) stepsToZ)
        else
            -- Example...
            PRNum $ product stepsToZ

