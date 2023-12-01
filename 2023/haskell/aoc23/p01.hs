import System.IO
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe

--
-- Data reading
--

readData :: FilePath -> IO [String]
readData fname = do
        handle <- openFile fname ReadMode
        contents <- hGetContents handle
        return (lines contents)

--
-- Part A
--

getDigits xs =
    (findDigit xs) * 10 + (findDigit (reverse xs))
  where findDigit = read . (:[]) . head . (dropWhile (not . isDigit))

solveA :: [String] -> Int
solveA inlines =
    sum (map getDigits inlines)

--
-- Part B
--

digitMaps :: [(Int, String)]
digitMaps =
    [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five"), (6, "six"),
     (7, "seven"), (8, "eight"), (9, "nine")]
    ++
    map (\i -> (i, show i)) [0..9]

-- If prefix is a prefix of xs, return Just (result, xs after prefix)
matchPrefix :: String -> (Int, String) -> Maybe (Int, String)
matchPrefix xs (result, prefix)
    | prefix `isPrefixOf` xs = Just (result, drop (length prefix) xs)
matchPrefix _ _ = Nothing

-- Return the first Just in the list
findJust [] = Nothing
findJust (x:_)
    | isJust x = x
findJust (_:xs) = findJust xs 

-- Find the first digit in the string xs and return (value, rest of string)
matchDigit :: String -> Maybe (Int, String)
matchDigit [] = Nothing
matchDigit xs =
    case findJust $ map (matchPrefix xs) digitMaps of
        Nothing ->
            matchDigit (tail xs)
        x -> x

-- Find a list of all digits in the string
matchAllDigits :: String -> [Int] -> [Int]
matchAllDigits [] acc = reverse acc
matchAllDigits xs acc =
    case matchDigit xs of
        Nothing -> reverse acc
        Just (d, xxs) -> matchAllDigits xxs (d:acc)

-- Get the "digit" of a string according to B's definition
getDigitsBAll :: String -> Int
getDigitsBAll xs =
    let digits = matchAllDigits xs []
    in
        (head digits) * 10 + (last digits)

solveB :: [String] -> Int
solveB inlines =
    sum (map getDigitsBAll inlines)


main = do
    t <- readData "input/p01_test.txt"
    putStr $ "Part A test "  ++ show (solveA t) ++ "\n"

    t <- readData "input/p01.txt"
    putStr $ "Part A "  ++ show (solveA t) ++ "\n"

    t <- readData "input/p01_test2.txt"
    putStr $ "Part B test "  ++ show (solveB t) ++ "\n"

    t <- readData "input/p01.txt"
    putStr $ "Part B "  ++ show (solveB t) ++ "\n"
