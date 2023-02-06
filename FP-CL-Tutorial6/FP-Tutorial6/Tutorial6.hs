{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Tutorial6 where

import Data.List (nub, isInfixOf)
import Control.Monad (liftM, liftM2)
import Test.QuickCheck (quickCheck, quickCheckAll,
                        Arbitrary(arbitrary), oneof, sized)
import GHC.CmmToAsm.AArch64.Instr (x0)

-- ** Implementing propositional logic in Haskell

-- Variable names
type Name = String

-- The datatype Prop
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :||: Prop
          | Prop :&&: Prop
          -- begin
          | Prop :->: Prop
          | Prop :<->: Prop
          -- end
          deriving (Eq)

-- Show
par :: String -> String
par s  =  "(" ++ s ++ ")"

showProp :: Prop -> String
showProp (Var x)     = x
showProp F           = "F"
showProp T           = "T"
showProp (Not p)     = par ("not " ++ showProp p)
showProp (p :||: q)  = par (showProp p ++ " || " ++ showProp q)
showProp (p :&&: q)  = par (showProp p ++ " && " ++ showProp q)
-- begin
showProp (p :->: q)  = par (showProp p ++ " -> " ++ showProp q)
showProp (p :<->: q) = par (showProp p ++ " <-> " ++ showProp q)
-- end

-- Valuations
type Valn = Name -> Bool

-- evaluates a proposition in a given valuation
eval :: Valn -> Prop -> Bool
eval vn (Var x)     = vn x
eval vn F           = False
eval vn T           = True
eval vn (Not p)     = not (eval vn p)
eval vn (p :||: q)  = eval vn p || eval vn q
eval vn (p :&&: q)  = eval vn p && eval vn q
-- begin
eval vn (p :->: q)  | eval vn p == True && eval vn q == False = False
                    | otherwise                               = True
eval vn (p :<->: q) | eval vn p == eval vn q = True
                    | otherwise              = False
-- end

-- list the variable names that occur in a proposition
-- the names in the result must be unique
type Names = [Name]

names :: Prop -> Names
names (Var x)     = [x]
names (F)         = []
names (T)         = []
names (Not p)     = names p
names (p :||: q)  = nub (names p ++ names q)
names (p :&&: q)  = nub (names p ++ names q)
-- begin
names (p :->: q)  = nub (names p ++ names q)
names (p :<->: q) = nub (names p ++ names q)
-- end

-- creates all possible valuations for a set of variable names
empty :: Valn
empty = error "undefined"

extend :: Valn -> Name -> Bool -> Valn
extend vn x b y | x == y    = b
                | otherwise = vn y

valns :: Names -> [Valn]
valns []     = [ empty ]
valns (x:xs) = [ extend vn x b
                 | vn <- valns xs, b <- [True, False] ]

-- checks whether a proposition is satisfiable
satisfiable :: Prop -> Bool
satisfiable p = or [ eval e p | e <- valns (names p) ]

-- ** Exercises

-- 1.
p1, p2, p3 :: Prop
p1 = (Var "P" :||: Not (Var "P") )
p2 = ((Var "P" :||: Var "Q") :&&: (Var "P" :&&: Var "Q"))
p3 = (Var "P" :&&: (Var "Q" :||: Var "R") :&&: (( Not (Var "P") :||: Not (Var "Q")) :&&: (Not (Var "P") :||: Not (Var "R"))))

{-

> table p1
P | (P || (not P))
- | --------------
1 |       1       
0 |       1   

> table p2
P Q | ((P || Q) && (P && Q))
- - | ----------------------
1 1 |           1           
0 1 |           0           
1 0 |           0           
0 0 |           0

> table p3
P Q R | ((P && (Q || R)) && (((not P) || (not Q)) && ((not P) || (not R))))
- - - | -------------------------------------------------------------------
1 1 1 |                                  0                                 
0 1 1 |                                  0                                 
1 0 1 |                                  0                                 
0 0 1 |                                  0                                 
1 1 0 |                                  0                                 
0 1 0 |                                  0                                 
1 0 0 |                                  0                                 
0 0 0 |                                  0    

> satisfiable p1
True

> satisfiable p2
True

> satisfiable p3
False

-}

-- 2. 
tautology :: Prop -> Bool
tautology p = and [ eval vn p | vn <- valns (names p) ]

prop_taut :: Prop -> Bool
prop_taut p = not (satisfiable (Not p)) == tautology p

{-
> tautology p1
True

> tautology p2
False

>tautology p3
False
-}

-- 3.
p4, p5, p6 :: Prop
p4 = ((Var "P" :->: Var "Q") :<->: (Not (Var "P") :||: Var "Q"))
p5 = ((Var "P" :->: Var "Q") :&&: (Var "Q" :->: Var "P"))
p6 = (((Var "P" :->: Var "Q") :&&: (Var "Q" :->: Var "R")) :&&: (Not (Var "P" :->: Var "R")))

{- (b)
> table (Var "P" :->: Var "Q")
P Q | (P -> Q)
- - | --------
1 1 |    1    
0 1 |    1    
1 0 |    0    
0 0 |    1   

> table (Var "P" :<->: Var "Q")
P Q | (P <-> Q)
- - | ---------
1 1 |     1    
0 1 |     0    
1 0 |     0    
0 0 |     1  
-}

{- (c)
> table p4
P Q | ((P -> Q) <-> ((not P) || Q))
- - | -----------------------------
1 1 |               1              
0 1 |               1              
1 0 |               1              
0 0 |               1  

> table p5
P Q | ((P -> Q) && (Q -> P))
- - | ----------------------
1 1 |           1           
0 1 |           0           
1 0 |           0           
0 0 |           1 

> table p6
P Q R | (((P -> Q) && (Q -> R)) && (not (P -> R)))
- - - | ------------------------------------------
1 1 1 |                     0                     
0 1 1 |                     0                     
1 0 1 |                     0                     
0 0 1 |                     0                     
1 1 0 |                     0                     
0 1 0 |                     0                     
1 0 0 |                     0                     
0 0 0 |                     0    

> satisfiable p4
True

> satisfiable p5
True

> satisfiable p6
False

> tautology p4
True

> tautology p5
False

> tautology p6
False
-}


-- ** Optional Material

-- 5.
isNNF :: Prop -> Bool
isNNF (Var x) = True
isNNF (Not p) = isNNF p
isNNF (p :||: q) = isNNF p && isNNF q
isNNF (p :&&: q) = isNNF p && isNNF q
isNNF (p :->: q) = False
isNNF (p :<->: q) = False

--not (isInfixOf "->" (showProp p) || isInfixOf "<->" (showProp p))

-- 6.
impElim :: Prop -> Prop
impElim (Var x) = Var x
impElim (Not p) = Not (impElim p)
impElim (p :||: q) = impElim p :||: impElim q
impElim (p :&&: q) = impElim p :&&: impElim q
impElim (p :->: q) = Not (impElim p) :||: impElim q
impElim (p :<->: q) = (Not (impElim p) :||: impElim q) :&&: (Not (impElim q) :||: impElim p)

--7.
notElim :: Prop -> Prop
notElim (Var x)          = Var x
notElim T                = T
notElim F                = F
notElim (p :||: q)       = notElim p :||: notElim q
notElim (p :&&: q)       = notElim p :&&: notElim q
notElim (Not (p :||: q)) = Not (notElim p) :&&: Not (notElim q)
notElim (Not (p :&&: q)) = Not (notElim p) :||: Not (notElim q)
notElim (Not p)          = Not (notElim p)


--8.
toNNF :: Prop -> Prop
toNNF p | not(isNNF p) = notElim (impElim p)
        | otherwise    = p

-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Prop -> Bool
prop_NNF1 p = isNNF (toNNF p)

-- check if result of toNNF is equivalent to its input
prop_NNF2 :: Prop -> Bool
prop_NNF2 p = equivalent p (toNNF p)

-- ** Challenge

-- 9.
isCNF :: Prop -> Bool
isCNF = undefined

-- 10.
fromLists :: [[Prop]] -> Prop
fromLists = undefined

-- 11.
toLists :: Prop -> [[Prop]]
toLists = undefined

-- 12.
toCNF :: Prop -> Prop
toCNF = undefined

-- check if result of toCNF is in CNF
prop_CNF1 :: Prop -> Bool
prop_CNF1 f = isCNF (toCNF f)

-- check if result of toCNF is equivalent to its input
prop_CNF2 :: Prop -> Bool
prop_CNF2 p = equivalent p (toCNF p)


-- ** Drawing Tables

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s = replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s = replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False = "0"
fort True  = "1"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab =
  putStr (
    unlines [ unwords (zipWith centre widths row) | row <- tab ]
  )
  where
    widths  = map length (head tab)

table :: Prop -> IO ()
table p = tables [p]

tables :: [Prop] -> IO ()
tables ps  =
  let xs = nub (concatMap names ps) in
   showTable (
     [ xs ++ ["|"] ++ [show p | p <- ps]           ] ++
     [ dashvars xs ++ ["|"] ++ [dash (show p) | p <- ps ]   ] ++
     [ evalvars vn xs ++ ["|"] ++ [fort (eval vn p) | p <- ps ] | vn <- valns xs]
     )
  where  dashvars xs    = [ dash x | x <- xs ]
         evalvars vn xs = [ fort (eval vn (Var x)) | x <- xs ]


-- ** For QuickCheck

equivalent :: Prop -> Prop -> Bool
equivalent p q = tautology ((p :&&: q) :||: (Not p :&&: Not q))

instance Show Prop where
  show = showProp

instance Arbitrary Prop where
  arbitrary = sized prop
      where
        prop n | n <= 0    = oneof [ return (Var "a"),
                                     return (Var "b"),
                                     return (Var "c"),
                                     return (Var "d"),
                                     return (Var "e"),
                                     return (Var "f")
                                   ]
               | otherwise = oneof [ liftM Var arbitrary
                                   , liftM Not p2
                                   , liftM2 (:||:) p2 p2
                                   , liftM2 (:&&:) p2 p2
                                   , liftM2 (:->:) p2 p2
                                   , liftM2 (:<->:) p2 p2
                                  ]
               where
                 p2  =  prop (n `div` 4)
