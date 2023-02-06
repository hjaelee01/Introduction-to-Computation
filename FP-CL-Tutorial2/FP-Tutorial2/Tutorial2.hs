module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck


-- 1. inRange

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x <- xs, lo <= x && x <= hi ]


-- 2. multDigits

multDigits :: String -> Int
multDigits str = product [ digitToInt x | x <- str, isDigit x]

countDigits :: String -> Int
countDigits str = length [ x | x<- str, isDigit x]

prop_multDigits :: String -> Bool
prop_multDigits str = multDigits str <= 9 ^ countDigits str


-- 3. capitalise and title

capitalise :: String -> String
capitalise (x:xs) = [toUpper x] ++ [toLower y | y <- xs]

title :: [String] -> [String]
title xs = [ if a == xs !! 0 then capitalise a else general a | a <- xs ]
    where general :: String -> String
          general y = if length y >= 4 then capitalise y else lowerAll y

          lowerAll :: String -> String
          lowerAll xs = [ toLower x | x <- xs ]
-- 4. score and totalScore

score :: Char -> Int
score x
    | isUpper x && x `elem` ['A', 'E', 'I', 'O', 'U'] = 3
    | (isLower x && x `elem` ['a', 'e', 'i', 'o', 'u']) || isUpper x = 2
    | isLower x || isDigit x = 1
    | otherwise = 0

totalScore :: String -> Int
totalScore xs = product [ score x | x <- xs ]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1


-- ** Optional Material

-- 5. crosswordFind

crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [ word | word <- words, length word == len, word !! pos == letter ]


-- 6. search


search :: String -> Char -> [Int]
search str goal = [ n | n <- [0..(length str -1)], tuplist !! n == theTuple ]
    where tuplist = zip str (replicate (length str) goal) -- [('B', 'o'), ('o', 'o'), ('o', 'o'), ('k', 'o')]
          theTuple = (goal, goal)


-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = length str >= length (search str goal)

