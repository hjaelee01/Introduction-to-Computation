module Tutorial1 where

import PicturesSVG -- needed for the optional chess part
import Test.QuickCheck


-- 2.
double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x * x

-- 3.
isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = square a + square b == square c

-- 4.
leg1 :: Int -> Int -> Int
leg1 x y = square x - square y

leg2 :: Int -> Int -> Int
leg2 x y = 2 * x * y

hyp :: Int -> Int -> Int
hyp x y = square x + square y

-- 5.
prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)

-- 7.
pic1 :: Picture
pic1 = undefined

pic2 :: Picture
pic2 = undefined

-- ** Functions

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)

-- 8.
twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoBeside (twoAbove x)

-- 9.
-- a)
emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

-- b)
otherEmptyRow :: Picture
otherEmptyRow = flipV emptyRow

-- c)
middleBoard :: Picture
middleBoard = repeatV 2 (above emptyRow otherEmptyRow)

-- d)
pieces :: Picture
pieces = beside rook (beside knight (beside bishop (beside queen (beside king (beside bishop (beside knight rook))))))

whiteRow :: Picture
whiteRow = over pieces otherEmptyRow

blackRow :: Picture
blackRow = over (invert pieces) emptyRow

-- e)
whitePawns :: Picture
whitePawns = over (repeatH 8 pawn) emptyRow

blackPawns :: Picture
blackPawns = over (repeatH 8 (invert pawn)) otherEmptyRow

populatedBoard :: Picture
populatedBoard = above (above (above (above blackRow blackPawns) middleBoard) whitePawns) whiteRow

squares :: [Int] -> [Int]
squares xs = [ x * x | x <- xs ]