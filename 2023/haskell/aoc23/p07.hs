module P07 where
import Common (ProbResult(PRNum), observe)
import Data.List (singleton, delete, sortOn, sortBy)
import Data.Tuple (swap)
import Data.Maybe (fromJust)
import Data.Ord (Down(..))

-- CJoker: CJ is converted into CJoker in part B
-- This way joker is lowest card in B
data Card = CJoker | C2 | C3 | C4 | C5 | C6 |
            C7 | C8 | C9 | CT | CJ |
            CQ | CK | CA
        deriving (Show, Eq, Ord)

instance Read Card where
    readsPrec _ [] = []
    readsPrec _ (x:xs) =
        case lookup x nameMap of
            Just c -> [(c, xs)]
            Nothing -> []
        where
            nameMap = [('A', CA), ('K', CK), ('Q', CQ), ('J', CJ), ('T', CT),
                       ('9', C9), ('8', C8), ('7', C7), ('6', C6), ('5', C5),
                       ('4', C4), ('3', C3), ('2', C2)]

data Hand = Hand {cards :: [Card], bid :: Int, rank :: Maybe Int}
    deriving (Show)

data HandType = High | Pair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
    deriving (Show, Eq, Ord)

-- Equivalent to "SELECT card, count(*) AS cnt GROUP BY card ORDER BY cnt DESC"
groupAndCount :: [Card] -> [(Card, Int)]
groupAndCount cards =
    sortOn (Down . swap) $ inner cards []
    where
        inner [] acc = acc
        inner (c:cs) acc =
            case lookup c acc of
                Nothing -> inner cs ((c, 1):acc)
                Just v -> inner cs ((c, v+1):(delete (c, v) acc))

handType :: [Card] -> HandType
handType cards =
    case groupAndCount cards of
        [(_, 5)] -> FiveKind
        [(_, 4), _] -> FourKind
        [(_, 3), (_, 2)] -> FullHouse
        [(_, 3), _, _] -> ThreeKind
        [(_, 2), (_, 2), _] -> TwoPair
        [(_, 2), _, _, _] -> Pair
        [_, _, _, _, _] -> High

handCompare :: Ord a => ([Card] -> a) -> Hand -> Hand -> Ordering
handCompare handType h1 h2
    | h1type < h2type = LT
    | h1type > h2type = GT
    | otherwise = compare (cards h1) (cards h2)
    where
        h1type = handType $ cards h1
        h2type = handType $ cards h2

------------------------------------------------------------------------------------------------
-- Data reading
------------------------------------------------------------------------------------------------

readHand :: String -> Hand
readHand s =
    Hand {cards = map (read . singleton) $ take 5 s ,
          bid = read (drop 6 s),
          rank = Nothing}

readData = map readHand

------------------------------------------------------------------------------------------------
-- Part A
------------------------------------------------------------------------------------------------

solveA dta =
    let hands = [h {rank = Just i} | (h, i) <- zip (sortBy (handCompare handType) (readData dta)) [1..]]
    in
    PRNum $ sum $ map (\h -> bid h * fromJust (rank h)) hands

------------------------------------------------------------------------------------------------
-- Part B
------------------------------------------------------------------------------------------------

handTypeB :: [Card] -> HandType
handTypeB cards =
    case groupAndCount cards of
        [(_, 5)] -> FiveKind

        [(a, 4), (b, _)]
            | a == CJoker || b == CJoker -> FiveKind
            | otherwise -> FourKind

        [(a, 3), (b, 2)]
            | a == CJoker || b == CJoker -> FiveKind
            | otherwise -> FullHouse

        [(a, 3), (b, _), (c, _)]
            | a == CJoker || b == CJoker || c == CJoker -> FourKind
            | otherwise -> ThreeKind

        [(a, 2), (b, 2), (c, _)]
            | a == CJoker || b == CJoker -> FourKind
            | c == CJoker -> FullHouse
            | otherwise -> TwoPair

        dta@[(_, 2), _, _, _]
            | CJoker `elem` map fst dta -> ThreeKind
            | otherwise -> Pair

        dta@[_, _, _, _, _]
            | CJoker `elem` map fst dta -> Pair
            | otherwise -> High

-- Replace CJ with CJoker. Used to make part B work
convertJokers :: [Hand] -> [Hand]
convertJokers =
    map convJ
    where
        convJ hand =
            hand {cards = map cjToJok (cards hand)}
        cjToJok CJ = CJoker
        cjToJok o = o


solveB dta =
    let hands = [h {rank = Just i} | (h, i) <- zip (sortBy (handCompare handTypeB) (convertJokers $ readData dta)) [1..]]
    in
    PRNum $ sum $ map (\h -> bid h * (fromJust $ rank h)) hands
