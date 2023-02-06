module Tutorial4 where

import Data.Char
import Data.List
import Test.QuickCheck


-- ** Optional material

-- 1. doubles
-- a.
doublesComp :: [Int] -> [Int]
doublesComp xs = [ 2 * x | x <- xs ] 

-- b.
doublesRec :: [Int] -> [Int]
doublesRec [] = []
doublesRec (x:xs) = 2*x : doublesRec xs 

-- c.
doublesHO :: [Int] -> [Int]
doublesHO = map (2*)

-- d.
prop_doubles :: [Int] -> Bool
prop_doubles xs =  doublesComp xs == doublesRec xs && doublesRec xs == doublesHO xs

-- 2. aboves
-- a.
abovesComp :: Int -> [Int] -> [Int]
abovesComp x xs =  [ a | a <- xs, a > x ]

-- b.
abovesRec :: Int -> [Int] -> [Int]
abovesRec n []     = []
abovesRec n (x:xs) | x > n     = x : abovesRec n xs
                   | otherwise = abovesRec n xs

-- c.
abovesHO :: Int -> [Int] -> [Int]
abovesHO n xs =  filter (>n) xs

-- d.
prop_aboves :: Int -> [Int] -> Bool
prop_aboves n xs =  abovesComp n xs == abovesRec n xs && abovesRec n xs == abovesHO n xs

-- 3. parity
-- a.
xor :: Bool -> Bool -> Bool
xor a b | a /= b    = True
        | otherwise = False

-- b.
parityRec :: [Bool] -> Bool
parityRec [] = True
parityRec (x:xs) = x `xor` parityRec xs

-- c.
parityHO :: [Bool] -> Bool
parityHO xs = foldr xor True xs

-- d.
prop_parity :: [Bool] -> Bool
prop_parity xs = parityRec xs == parityHO xs

-- 4. allcaps
-- a.
allcapsComp :: String -> Bool
allcapsComp xs = and [ isUpper x | x <- xs, isAlpha x ] 

-- b.
allcapsRec :: String -> Bool
allcapsRec []     = True
allcapsRec (x:xs) = isUpper x && allcapsRec xs

-- c.
allcapsHO :: String -> Bool
allcapsHO xs =  foldr (&&) True (map isUpper (filter isAlpha xs))

-- d.
prop_allcaps :: String -> Bool
prop_allcaps xs =  allcapsComp xs == allcapsRec xs && allcapsRec xs == allcapsHO xs


-- ** Optional material
-- Matrix manipulation

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform xs = all (== head xs) xs

-- b.
valid :: Matrix -> Bool
valid xs = uniform [ length x | x <- xs ] && and [length x /= 0 | x <- xs ]


-- 6.
width :: Matrix -> Int
width m = length (head m)

height :: Matrix -> Int
height m = length m

plusM :: Matrix -> Matrix -> Matrix
plusM mx1 mx2 = [ zipWith (+) a b | (a,b) <- zip mx1 mx2
                                     ,valid mx1 && valid mx2, width mx1 == width mx2, height mx1 == height mx2 ]

-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM mx1 mx2 = [ [ dot row1 col2 | col2 <- tx2 ] | row1 <- mx1 ]
        where tx2 = transpose mx2

              dot :: [Rational] -> [Rational] -> Rational
              dot ma1 ma2 = sum (zipWith (*) ma1 ma2)


-- ** Challenge

-- 8.
-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined
        
determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)
        
prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined
