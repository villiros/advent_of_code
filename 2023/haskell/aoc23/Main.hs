{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.IO
import System.Console.CmdArgs
import Control.Exception (assert)
import Text.Printf (printf)
import Data.Time.Clock.POSIX (getPOSIXTime, getCurrentTime)

import Common
import qualified P01 
import qualified P02
import qualified P03
import qualified P04
import qualified P05
import qualified P06
import qualified P07
import qualified P08
import qualified P09
import qualified P10
import qualified P11
import Data.Time (diffUTCTime)
import qualified P12

nameToSolve n = case n of
    "p01a" -> P01.solveA
    "p01b" -> P01.solveB
    "p02a" -> P02.solveA
    "p02b" -> P02.solveB
    "p03a" -> P03.solveA
    "p03b" -> P03.solveB
    "p04a" -> P04.solveA
    "p04b" -> P04.solveB
    "p05a" -> P05.solveA
    "p05b" -> P05.solveB
    "p06a" -> P06.solveA
    "p06b" -> P06.solveB
    "p07a" -> P07.solveA
    "p07b" -> P07.solveB
    "p08a" -> P08.solveA
    "p08b" -> P08.solveB
    "p09a" -> P09.solveA
    "p09b" -> P09.solveB
    "p10a" -> P10.solveA
    "p10b" -> P10.solveB
    "p11a" -> P11.solveA
    "p11b" -> P11.solveB
    "p12a" -> P12.solveA
    "p12b" -> P12.solveB


data Answer =
    Answer {
        probname :: String,
        fname :: String,
        result :: ProbResult,
        resultKnown :: Bool
    } deriving (Show)

readAnswers ansFName = do
    handle <- openFile ansFName ReadMode
    contents <- hGetContents handle
    return $ foldr readAns [] $ map words $ lines contents
    where
        readAns [] acc = acc
        readAns (('#':_):_) acc = acc
        readAns [pname, fname, ans] acc =
            let (answer, resultKnown) = case ans of
                    "?" -> (PRNum 0, False)
                    '"':_ -> (PRStr (read ans), True)
                    _ -> (PRNum (read ans), True)
            in          
            Answer {probname = pname,
                    fname = fname,
                    result = answer,
                    resultKnown = resultKnown} : acc

data CmdParams =
    CmdParams {probs :: [String], verbose :: Bool}
    deriving (Show, Data, Typeable)

cmdParams = CmdParams {probs = [], verbose = False}

filterAnswersFromCmd CmdParams{probs = []} ans = ans
filterAnswersFromCmd CmdParams{probs = names} ans =
    concat
    [case filter ((== a) . probname) ans of
        [] -> error ("Invalid prob name: " ++ a)
        answs -> answs
    | a <- names]


runCase :: Answer -> IO Bool
runCase ans = do
    printf "# Running %s on %s" (probname ans) (fname ans)
    if resultKnown ans then
        printf " expect result %s\n" (show $ result ans)
    else
        putStrLn ""
    handle <- openFile ("input/" ++ (fname ans)) ReadMode
    contents <- hGetContents handle

    startTime <- getCurrentTime
    let solveResult = (nameToSolve $ probname ans) (lines contents)
    endTime <- getCurrentTime
    let timeTakenMs = round ((diffUTCTime endTime startTime) * 1000) :: Int

    if not $ resultKnown ans then do
        printf " ???: Got %s Time: %i ms\n" (show solveResult) timeTakenMs
        return True
    else if solveResult == result ans then do
        printf " OK Time: %ims\n" timeTakenMs
        return True
    else do
        printf " FAIL: Got %s Expected %s Diff %s Time: %ims\n"
            (show solveResult)
            (show $ result ans)
            (case result ans of
                PRNum en -> show (en - fromPRNum solveResult)
                _ -> "???")
            timeTakenMs
        return False

main :: IO ()
main = do    
    args <- cmdArgs cmdParams
    answers0 <- readAnswers "answers"
    let answers = filterAnswersFromCmd args answers0
    results <- sequence (map runCase answers)
    if all id results then do
        putStrLn "ALL OK"
    else do
        putStrLn "FAILURES"

