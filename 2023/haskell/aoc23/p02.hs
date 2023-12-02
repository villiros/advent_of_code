import System.IO
import Data.List (isPrefixOf)
import Debug.Trace

findIndex :: (Eq a) => [a] -> [a] -> Maybe Int
findIndex search =
    findInner 0
    where findInner n s
            | null s = Nothing
            | search `isPrefixOf` s = Just n
            | otherwise = findInner (n+1) (tail s)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn pat str =
    case findIndex pat str of
        Nothing -> [str]
        Just i -> (take i str):splitOn pat (drop (i + length pat) str)

------------------------------------------------------------------------------------------------
--
-- Data reading
--
------------------------------------------------------------------------------------------------

data GameRound = GameRound {red :: Int,
                            green :: Int,
                            blue :: Int} deriving (Show)

data Game = Game {gid :: Int,
                  rounds :: [GameRound]} deriving (Show)

updateGame g ln =
    case splitOn " " ln of
        ["", nStr, "red"] -> g {red = read nStr}
        ["", nStr, "green"] -> g {green = read nStr}
        ["", nStr, "blue"] -> g {blue = read nStr}

parseGame ln =
    foldl updateGame (GameRound 0 0 0) (splitOn "," ln)

parseLine :: String -> Game
parseLine ln =
    let [gm, rest] = splitOn ":" ln
        [_, idStr] = splitOn " " gm
        gamesStrs = splitOn ";" rest
    in
        Game {gid = read idStr, rounds = map parseGame gamesStrs}


readData :: FilePath -> IO [Game]
readData fname = do
    handle <- openFile fname ReadMode
    contents <- hGetContents handle
    return $ map parseLine (lines contents)

------------------------------------------------------------------------------------------------
--
-- Part A
--
------------------------------------------------------------------------------------------------

validRoundA GameRound{red = r, green = g, blue = b} =
    r <= 12 && g <= 13 && b <= 14

solveA =
    sum . map gid . filter (all validRoundA . rounds)

------------------------------------------------------------------------------------------------
--
-- Part A
--
------------------------------------------------------------------------------------------------

roundMax GameRound{red = ar, green = ag, blue = ab}
         GameRound{red = r, green = g, blue = b} =
            GameRound{red = max r ar, green = max ag g, blue = max ab b}

solveB :: [Game] -> Int
solveB =
    sum . map power . map getFewest
    where power GameRound{red = r, green = g, blue = b} = r * g * b
          getFewest = foldl roundMax (GameRound 0 0 0) . rounds

------------------------------------------------------------------------------------------------
--
-- main
--
------------------------------------------------------------------------------------------------

main = do
    t <- readData "input/p02_test.txt"
    putStr $ "Part A test " ++ show (solveA t) ++ "\n"

    t <- readData "input/p02.txt"
    putStr $ "Part A " ++ show (solveA t) ++ "\n"

    t <- readData "input/p02_test.txt"
    putStr $ "Part B test " ++ show (solveB t) ++ "\n"

    t <- readData "input/p02.txt"
    putStr $ "Part B " ++ show (solveB t) ++ "\n"
