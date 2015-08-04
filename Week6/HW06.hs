{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module HW06 where

import Data.List

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib n
    | n < 1 = error "Fibonacci sequence undefined for n < 1"
    | n <= 2 = 1
    | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a (s)) = a : streamToList s

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons a (s)) = Cons (f a) (fmap f s)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat a = Cons a (sRepeat a)

sIterate :: (a -> a) -> a -> Stream a
sIterate f a = Cons a (sIterate f $ f a)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons a s1) s2 = Cons a (sInterleave s2 s1)

sTake :: Int -> Stream a -> [a]
sTake n s = take n $ streamToList s

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 1

ruler :: Stream Integer
ruler = fmap rulerSeq nats
    where rulerSeq n = if odd n then 0 else 1 + rulerSeq (n `div` 2)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = sIterate (\n -> (1103515245*n+12345) `mod` 2147483648) seed

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 235 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 164 MB -}
minMax:: [Int] -> Maybe (Int, Int)
minMax [] = Nothing   -- no min or max if there are no elements
minMax l = go Nothing l
    where go acc [] = acc
          go Nothing (d:xs) = go (Just (d, d)) xs
          go (Just (y, z)) (d:xs) = (Just (y, z)) `seq` go (Just (min y d, max y d)) xs


main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix = Matrix Integer Integer Integer Integer
    deriving (Show)

instance Num Matrix where
    (Matrix a11 a12 a21 a22) * (Matrix b11 b12 b21 b22) =
        Matrix (a11*b11+a12*b21) (a11*b12+a12*b22) (a21*b11+a22*b21) (a21*b12+a22*b22)

fib_base :: Matrix
fib_base = Matrix 1 1 1 0

fastFib :: Int -> Integer
fastFib 0 = 1
fastFib n = case (fib_base ^ n) of
    (Matrix out _ _ _) -> out
