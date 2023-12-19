module Common where

import Debug.Trace (traceShow)

data ProbResult =
    PRNum Int | PRStr String
    deriving (Eq)

instance Show ProbResult where
    show (PRNum n) = show n
    show (PRStr s) = show s

fromPRNum (PRNum n) = n

observe :: (Show a, Show b) => a -> b -> b
observe a b =
--    b
    traceShow (a, b) $ b

observeIf shouldShow a b =
    if shouldShow then  observe a b else b

listSplit :: (a -> Bool) -> [a] -> [[a]]
listSplit pred xs =
    case break pred xs of
        ([], _:xxs) -> listSplit pred xxs
        ([], []) -> []
        (x, []) -> [x]
        (x, _:xxs) -> x:listSplit pred xxs

flatten [] = []
flatten (x:xs) = x ++ flatten xs