module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)

prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- ** Caesar Cipher Exercises

-- 1.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp a dic = if length f > 0 then head f else a 
      where f = [ snd el | el <- dic, fst el == a ]
lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec a [] = a
lookUpRec a (x:xs) | fst x == a = snd x
                   | otherwise  = lookUp a xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp a dic = lookUp a dic == lookUpRec a dic

-- 2.
encipher :: Int -> Char -> Char
encipher n a = lookUp a (makeKey n)

-- 3.
normalise :: String -> String
normalise str = [ toUpper x | x <- str, isAlpha x ]

normaliseRec :: String -> String
normaliseRec [] = []
normaliseRec (x:xs) | isAlpha x = toUpper x : normaliseRec xs
                    | otherwise = normaliseRec xs

prop_normalise :: String -> Bool
prop_normalise str = normalise str == normaliseRec str

-- 4.
enciphers :: Int -> String -> String
enciphers n str = [ encipher n x | x <- normalise str ]

-- 5.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey lst = [ (snd x, fst x) | x <- lst ]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec []     = []
reverseKeyRec (x:xs) = (snd x, fst x) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey lst = reverseKey lst == reverseKeyRec lst

-- 6.
decipher :: Int -> Char -> Char
decipher n a = lookUp a (reverseKey (makeKey n)) -- reversed dictionary = reverseKey (makeKey n)

decipherStr :: Int -> String -> String
decipherStr n str = [ decipher n x | x <- str, isUpper x ]


-- ** Optional Material

-- 7.
candidates :: String -> [(Int, String)]
candidates str = [ x | x <- hf str, isInfixOf "THE" (snd x) || isInfixOf "AND" (snd x) ]
            where hf :: String -> [(Int, String)]
                  hf s = [ (n, decipherStr n s) | n <- [1..26] ]


splitEachFive :: String -> [String]
splitEachFive xs | length xs > 5 = take 5 xs : splitEachFive (drop 5 xs)
                 | otherwise     = [ fillToFive xs ]

fillToFive :: String -> String
fillToFive xs = xs ++ replicate (5 - length xs) 'X'

-- An alternative solution demonstrating 'repeat'
fillToFive' :: String -> String
fillToFive' xs = take 5 (xs ++ repeat 'X')

-- The following example shows why 'transpose' is not
--  invertible in general. The transpose function
--  takes the 'columns' of a list of lists, and makes
--  them the 'rows' of a new list of lists. 
--
-- [[o n e],           [[o t t f f],       [[o n e e e],
--  [t w o],            [n w h o i],        [t w o r],  
--  [t h r e e],   -->  [e o r u v],   -->  [t h r e],  
--  [f o u r],          [e r e],            [f o u], 
--  [f i v e]   ]       [e],        ]       [f i v]     ]   

-- 8.
encrypt :: Int -> String -> String
encrypt n [] = []
encrypt n xs = intercalate "" (transpose (splitEachFive (enciphers n xs)))

-- 9.
decrypt :: Int -> String -> String
decrypt n str = decipherStr n final
      where 
            final      = [ x | x <- semiOrigin, x /= 'X' ]
            semiOrigin = intercalate "" (transpose (unTranspose str))
            
            unTranspose :: String -> [String]
            unTranspose [] = [] 
            unTranspose xs = take takeNumber xs : unTranspose (drop takeNumber xs)

            takeNumber = length str `div` 5