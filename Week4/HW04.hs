{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P l) == (P r) = removeTrailing l == removeTrailing r
        
removeTrailing :: (Num a, Eq a) => [a] -> [a]
removeTrailing xs = reverse $ dropWhile (==0) $ reverse xs
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a, Ord a) => Show (Poly a) where
    show (P l) = printPoly l
    
printPoly :: (Num a, Eq a, Show a, Ord a) => [a] -> String
printPoly l = concat $ reverse $ intersperse " + " [ sign (fst t) ++ term (abs $ fst t) (snd t) | t <- zip l [0::Integer ..], fst t /= 0 ]
    where term 1 0 = "1"
          term 1 1 = "x"
          term 1 c = "x^" ++ show c
          term y 0 = show y
          term y 1 = show y ++ "x"
          term y c = show y ++ "x^" ++ show c
          sign :: (Num a, Ord a) => a -> String
          sign n
            | n >= 0 = ""
            | otherwise = "-"


-- Exercise 4 -----------------------------------------

zipWithDefault :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithDefault da db la lb = let len = max (length la) (length lb)
                                 la' = la ++ (repeat da)
                                 lb' = lb ++ (repeat db)
                             in take len $ zip la' lb'  

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P l) (P r) = P $ map (\(y, v) -> y+v) (zipWithDefault 0 0 l r)

mpscalar :: Num a => [a] -> a -> [a]
mpscalar l n = map (*n) l

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P l) (P r) = foldr plus (P [0]) $ map P [ shiftRight (fst t) (mpscalar l (snd t)) | t <- zip [0..] r ]
    where  shiftRight :: Num a => Int -> [a] -> [a]
           shiftRight n xs = replicate n 0 ++ xs

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P l) = P $ map (negate) l
    fromInteger n = P [fromIntegral n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P l) n = sum [ fst t * n ^ (snd t) | t <- zip l [0::Integer ..] ]

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 o = o
    nderiv n o = nderiv (n-1) (deriv o)

-- Exercise 9 -----------------------------------------

instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (P l) = P $ drop 1 $ [ fst t * snd t | t <- zip l [0..] ]

