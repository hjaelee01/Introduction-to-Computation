--Exercise 1
--Either B or D. 
--It’s only B that doesn’t have a bold line around it. But, the odd one out can also be D because it is the only blue one.


-- Exercise 2
import GHC.Driver.Flags (WarningFlag(Opt_WarnUnbangedStrictPatterns))
import GHC.Core (Unfolding(BootUnfolding))
data Thing = A | B | C | D | E deriving (Eq, Show)
things :: [Thing]
things = [A, B, C, D, E]
data Colour = Amber | Blue     deriving (Eq, Show)
data Shape = Square | Circle   deriving (Eq, Show)
data Size  = Big | Small       deriving (Eq, Show)
data Border  = Thick | Thin    deriving (Eq, Show)

colour :: Thing -> Colour
colour A = Amber
colour B = Amber
colour C = Amber
colour D = Blue
colour E = Amber

shape :: Thing -> Shape
shape A = Square
shape B = Square
shape C = Circle
shape D = Square
shape E = Square

size :: Thing -> Size
size A = Big
size B = Big
size C = Big 
size D = Big
size E = Small

bold :: Thing -> Border
bold A = Thick
bold B = Thin
bold C = Thick
bold D = Thick
bold E = Thick

-- Exercise 3

type Predicate u = u -> Bool

isBlue :: Predicate Thing
isBlue x = x `elem` [D]

isSquare :: Predicate Thing
isSquare x = x `elem` [A, B, D, E]

isBig :: Predicate Thing
isBig x = x `elem` [A, B, C, D]

isThin :: Predicate Thing
isThin x = x `elem` [B]

--Exercise 4
--1)
--and [ isThin x | x <- things, isBlue x && isSquare x ] == False

--2)
--or [ not (isBig x) | x <- things, not (isBlue x) && not (isSquare x) ] == False

--Exercise 5

--'It is not the case that some square is Blue'
-- not (or [ isBlue x | x <- things, isSquare x ])             -> result : False

--'Every square is not Blue'
--and [ not (isBlue x) | x <- things, isSquare x ]             -> result : False

--Exercise 6

thingsOtherThan :: Thing -> [Thing]
thingsOtherThan x = [ y | y <- things, y /= x ]

isAmber :: Predicate Thing
isAmber x = not (isBlue x)
isCircle :: Predicate Thing
isCircle x = not (isSquare x)
isSmall :: Predicate Thing
isSmall x = not (isSmall x)
isThick :: Predicate Thing
isThick x = not (isThin x)

properties :: [Predicate Thing]
properties = [ isBlue, isAmber, isSquare, isCircle, isBig, isSmall, isThin, isThick ]

propertiesOf :: Thing -> [Predicate Thing]
propertiesOf x = [ prop | prop <- properties, prop x ]


isPropertyOfAnotherThing :: Predicate Thing -> Thing -> Bool
isPropertyOfAnotherThing prop x = prop x == False

propertiesOnlyOf :: Thing -> [Predicate Thing]
propertiesOnlyOf x = [prop | prop <- properties, others <- thingsOtherThan x, prop x && (prop others == False)]

rank :: Thing -> Int
rank x = length (propertiesOnlyOf x)