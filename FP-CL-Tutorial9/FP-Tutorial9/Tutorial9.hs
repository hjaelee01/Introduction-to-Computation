module Tutorial9 where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Test.QuickCheck
import GHC.CmmToAsm.AArch64.Instr (x0)
import System.Console.Terminfo (Point(row))
import GHC.Core (Unfolding(BootUnfolding))

-- Representing Sudoku puzzles

type Row a    = [a]
type Matrix a = [Row a]
type Digit    = Char

-- Examples from websudoku.com

easy :: Matrix Digit
easy = ["    345  ",
        "  89   3 ",
        "3    2789",
        "2 4  6815",
        "    4    ",
        "8765  4 2",
        "7523    6",
        " 1   79  ",
        "  942    "]

medium :: Matrix Digit
medium = ["   4 6 9 ",
          "     3  5",
          "45     86",
          "6 2 74  1",
          "    9    ",
          "9  56 7 8",
          "71     64",
          "3  6     ",
          " 6 9 2   "]

hard :: Matrix Digit
hard = ["9 3  42  ",
        "4 65     ",
        "  28     ",
        "     5  4",
        " 67 4 92 ",
        "1  9     ",
        "     87  ",
        "     94 3",
        "  83  6 1"]

evil :: Matrix Digit
evil = ["  9      ",
        "384   5  ",
        "    4 3  ",
        "   1  27 ",
        "2  3 4  5",
        " 48  6   ",
        "  6 1    ",
        "  7   629",
        "     5   "]

-- Another example, from Bird's book

book :: Matrix Digit
book = ["  4  57  ",
        "     94  ",
        "36      8",
        "72  6    ",
        "   4 2   ",
        "    8  93",
        "4      56",
        "  53     ",
        "  61  9  "]

-- Printing Sudoku puzzles

group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy n [] = []
groupBy n xs = take n xs : groupBy n (drop n xs)

intersperse :: a -> [a] -> [a]
intersperse sep []     = [sep]
intersperse sep (y:ys) = sep : y : intersperse sep ys

showRow :: String -> String
showRow = concat . intersperse "|" . group

showGrid :: Matrix Digit -> [String]
showGrid = showCol . map showRow
  where
    showCol = concat . intersperse [bar] . group
    bar     = replicate 13 '-'

put :: Matrix Digit -> IO ()
put = putStrLn . unlines . showGrid

-- 1.
choice :: Digit -> [Digit]
choice d | d == ' ' = "123456789"
         | otherwise = [d]

choices :: Matrix Digit -> Matrix [Digit]
choices m = [take 9 (drop (9*k) bindLess) | k <- [0..8]]
        where bindLess = [ choice (row !! n) | row <- m, n <- [0..8] ]

              
-- 2.
splits :: [a] -> [(a, [a])]
splits xs  =
  [ (xs!!k, take k xs ++ drop (k+1) xs) | k <- [0..n-1] ]
  where
  n = length xs

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = [ if length (splits str) == 1 then str else refine str | str <- row]
        where refine :: [Digit] -> [Digit]
              refine s = concat [ [fst el] | el <- splits s, [fst el] `notElem` row ]


-- this code builds on pruneRow to also prune columns and boxes

pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

rows, cols, boxs :: Matrix a -> Matrix a
rows = id
cols = transpose
boxs = map ungroup . ungroup . map cols . group . map group
  where
    ungroup :: Matrix a -> [a]
    ungroup = concat

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy cols . pruneBy rows

-- 3.
close :: Eq a => (a -> a) -> a -> a
close g x | x == g x = x
          | otherwise      = close g $ g x

-- 4.

extract :: Matrix [Digit] -> Matrix Digit
extract mtx | all (all canExtract) mtx = map (map unlist) mtx
            | otherwise      = undefined
            where 
                unlist [a] = a
                canExtract x = length x == 1

-- 5.
solve :: Matrix Digit -> Matrix Digit
solve problem = extract $ close prune $ choices problem

{-

Only easy and medium can be solved this way.
ghci> put $ solve easy
-------------
|927|834|561|
|168|975|234|
|345|162|789|
-------------
|234|796|815|
|591|248|673|
|876|513|492|
-------------
|752|389|146|
|413|657|928|
|689|421|357|
-------------

ghci> put $ solve medium
-------------
|837|456|192|
|296|183|475|
|451|729|386|
-------------
|682|374|951|
|175|298|643|
|943|561|728|
-------------
|719|835|264|
|328|647|519|
|564|912|837|
-------------

ghci> put $ solve hard
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:74:14 in base:GHC.Err
  undefined, called at Tutorial9.hs:151:32 in main:Tutorial9

ghci> put $ solve evil
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:74:14 in base:GHC.Err
  undefined, called at Tutorial9.hs:151:32 in main:Tutorial9

-}
-- ** Optional Material

-- 6.
failed :: Matrix [Digit] -> Bool
failed matrix = or [null cell | row <- matrix, cell <- row]

-- 7.
solved :: Matrix [Digit] -> Bool
solved matrix = and [length cell == 1 | row <- matrix, cell <- row]

-- 8.
shortest :: Matrix [Digit] -> Int
shortest matrix = minimum $ filter (>1) $ map length $ concat matrix

-- 9.
expand :: Matrix [Digit] -> [Matrix [Digit]]
expand matrix = [ preMat ++ [preRow ++ [[d]] ++ postRow] ++ postMat | d <- ds ]
        where
                short ds = length ds == shortest matrix 
                (preMat, row:postMat) = break (any short) matrix
                (preRow, ds:postRow) = break short row
-- 10.
pruneMat :: Matrix [Digit] -> Matrix [Digit]
pruneMat = close prune

solveThorough :: Matrix [Digit] -> [Matrix Digit]
solveThorough matrix
        | solved $ pruneMat matrix = [extract $ pruneMat matrix]
        | failed $ pruneMat matrix = []
        | otherwise                = [ sol | exp <- expand (pruneMat matrix), sol <- solveThorough exp ]

search :: Matrix Digit -> [Matrix Digit]
search matrix = solveThorough $ choices matrix

-- display puzzle and then solution(s) found by search

puzzle :: Matrix Digit -> IO ()
puzzle g = put g >> puts (search g) >> putStrLn "***"
     where puts = sequence_ . map put
       
main :: IO ()
main = puzzle easy >>
       puzzle medium >>
       puzzle hard >>
       puzzle evil

