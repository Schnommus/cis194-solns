{-# OPTIONS_GHC -Wall #-}
module HW02 where
import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c1 c2 = length . filter (uncurry (==)) . zip c1 $ c2

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors c = [length . filter (==x) $ c | x <- colors]

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = sum [minimum [fst x, snd x] | x <- zip (countColors c1) (countColors c2)]

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove c1 c2 = Move c2 (exactMatches c1 c2) (matches c1 c2 - exactMatches c1 c2)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move c1 _ _) c2 = consistent m $ getMove c1 c2
  where consistent (Move _ e1 n1) (Move _ e2 n2) = e1 == e2 && n1 == n2

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m cs = filter (isConsistent m) cs

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 1 = map (:[]) colors
allCodes n = concat [ map (c:) (allCodes (n-1)) | c <- colors]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Code]
solve c = reduce [Red, Red, Red, Red, Red, Red] (allCodes (length c))

reduce g [] = []
reduce g t = g: reduce (head filtered) (tail filtered)
  where filtered = filterCodes (getMove g (head t)) t
-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
