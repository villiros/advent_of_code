module Common where

data ProbResult =
    PRNum Int | PRStr String
    deriving (Eq)

instance Show ProbResult where
    show (PRNum n) = show n
    show (PRStr s) = show s

fromPRNum (PRNum n) = n