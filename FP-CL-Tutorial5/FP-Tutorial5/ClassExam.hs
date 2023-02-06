-- Informatics 1 - Functional Programming 
-- Class Test 2022

module ClassExam where

import Data.Char
import Test.QuickCheck

-- Problem 1

-- a

f :: String -> Int
f xs = sum [ ord x | x <- xs, isAlpha x ]

-- b

g :: String -> Int
g [] = 0
g (x:xs) | isAlpha x = ord x + g xs
         | otherwise = g xs

-- c

h :: String -> Int
h xs = sum (map ord (filter isAlpha xs))

-- d

prop_fgh :: String -> Bool
prop_fgh xs = f xs == g xs && g xs == h xs

-- Problem 2

-- a

c :: String -> String -> Bool
c s1 s2 = and [ a == b | (a,b) <- (zip s1 s2), isAlpha a && isAlpha b ]

-- b

d :: String -> String -> Bool
d [] s2 = True
d s1 [] = True
d (a:as) (b:bs) | isAlpha a && isAlpha b && a==b = True && d as bs
                | isAlpha a && isAlpha b && a/=b = False && d as bs
                | otherwise              = d as bs

-- c

prop_cd :: String -> String -> Bool
prop_cd s1 s2 = c s1 s2 == d s1 s2
