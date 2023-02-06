{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Tutorial7 where

import LSystem
import Test.QuickCheck
import GHC.CmmToAsm.AArch64.Instr (x0)
import Data.Text (splitOn)

pathExample = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30

-- 1a. copy
copy :: Int -> Command -> Command
copy n cmd | n < 0  = error "n is negative"
           | n == 0 = Sit
           | n > 0  = cmd :#: copy (n-1) cmd

-- 1b. polygon
polygon :: Distance -> Int -> Command
polygon dist sides = copy sides (Go dist :#: Turn (360 / fromIntegral sides))

-- 2. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n :#: Sit
    where 
        f 0 = Go 10
        f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1)
        n = Turn 60
        p = Turn (-60)


-- 3. sierpinski
sierpinski :: Int -> Command
sierpinski x = f x
    where
        f 0 = GrabPen red :#: Go 10
        f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
        g 0 = GrabPen blue :#: Go 10
        g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
        n = Turn 60
        p = Turn (-60)
     
-- 4. hilbert
hilbert :: Int -> Command
hilbert =  undefined

-- 5. dragon
dragon :: Int -> Command
dragon =  undefined

-- ** Optional Material

-- 6a. split
split :: Command -> [Command]
split (x :#: xs) | x == Sit = split xs
                 | otherwise = split x ++ split xs

split x | x == Sit  = []
        | otherwise = [x]


-- 6b. join
join :: [Command] -> Command
join (x:xs) | null xs  = x
            | otherwise = x :#: join xs
join [] = Sit

-- 6c. equivalent
equivalent :: Command -> Command -> Bool
equivalent cmd1 cmd2 = split cmd1 == split cmd2

-- 6d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = equivalent (join (split c)) c

prop_split :: Command -> Bool
prop_split c = notElem Sit (split c) && notElem Sit (split c)

-- 7. optimise
optimise :: Command -> Command
optimise c = join(removeRepeat withOutSit)
    where
        withOutSit = split (refine c)

        refine :: Command -> Command
        refine (x :#: xs) | x == Go 0 = Sit :#: refine xs
                          | x == Turn 0 = Sit :#: refine xs
                          | otherwise = x :#: refine xs
        refine x = x
        
        removeRepeat :: [Command] -> [Command]
        removeRepeat [] = []
        removeRepeat (Go x : Go y : ys)     = Go (x+y) : removeRepeat ys
        removeRepeat (Turn x : Turn y : ys) = Turn (x+y) : removeRepeat ys
        removeRepeat (x:xs)                 = x : removeRepeat xs




--optimise c = 
--    where 
--        compact :: [Command] -> [Command]
--        compact (x:xs) | x == Go && head 

        

--        onlyRepeat = filter (\x -> x/=Go 0.0 && x/=Turn 0) c
