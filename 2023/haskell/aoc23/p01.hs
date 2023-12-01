import System.IO
import Data.Char (isDigit, intToDigit, digitToInt)
import Data.List (isPrefixOf, singleton)
import Data.Maybe

------------------------------------------------------------------------------------------------
--
-- Data reading
--
------------------------------------------------------------------------------------------------

readData :: FilePath -> IO [String]
readData fname = do
        handle <- openFile fname ReadMode
        contents <- hGetContents handle
        return (lines contents)

------------------------------------------------------------------------------------------------
--
-- Part A
--
------------------------------------------------------------------------------------------------

getDigits xs =
    (findDigit xs) * 10 + (findDigit (reverse xs))
  where findDigit = digitToInt . head . (dropWhile (not . isDigit))

solveA :: [String] -> Int
solveA inlines =
    sum (map getDigits inlines)

------------------------------------------------------------------------------------------------
--
-- Part B
--
------------------------------------------------------------------------------------------------

digitMaps :: [(Int, String)]
digitMaps =
    zip [1..9] ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    ++
    map (\x -> (x, [intToDigit x])) [0..9]

-- If prefix is a prefix of xs, return Just (result, xs after prefix)
matchPrefix :: String -> (Int, String) -> Maybe (Int, String)
matchPrefix xs (result, prefix) =
    if prefix `isPrefixOf` xs then  
        Just (result, drop (length prefix) xs)
    else
        Nothing

-- Find the first digit in the string xs and return (value, rest of string)
matchDigit :: String -> Maybe (Int, String)
matchDigit xs =
    case mapMaybe (matchPrefix xs) digitMaps of
        [] | xs /= [] -> matchDigit (tail xs)
        [] -> Nothing
        x:_ -> Just x

-- Find a list of all digits in the string
matchAllDigits :: String -> [Int]
matchAllDigits xs =
    case matchDigit xs of
        Nothing -> []
        Just (d, xxs) -> d : matchAllDigits xxs

-- Get the "digit" of a string according to B's definition
getDigitsBAll :: String -> Int
getDigitsBAll xs =
    (head digits) * 10 + (last digits)
    where digits = matchAllDigits xs

solveB :: [String] -> Int
solveB inlines =
    sum (map getDigitsBAll inlines)

------------------------------------------------------------------------------------------------
--
-- main
--
------------------------------------------------------------------------------------------------

main = do
    t <- readData "input/p01_test.txt"
    putStr $ "Part A test "  ++ show (solveA t) ++ "\n"

    t <- readData "input/p01.txt"
    putStr $ "Part A "  ++ show (solveA t) ++ "\n"

    t <- readData "input/p01_test2.txt"
    putStr $ "Part B test "  ++ show (solveB t) ++ "\n"

    t <- readData "input/p01.txt"
    putStr $ "Part B "  ++ show (solveB t) ++ "\n"
