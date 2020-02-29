module FoldableByListReplacements where

import Prelude (Int, Bool, Show, Float, Integral, Ord, Eq, Num)
import qualified Prelude as P

length :: [a] -> Int
length = P.length

concat :: [[a]] -> [a]
concat = P.concat

elem :: Eq a => a -> [a] -> Bool
elem = P.elem

sum :: Num a => [a] -> a
sum = P.sum

product :: Num a => [a] -> a
product = P.product

null ::  [a] -> Bool
null = P.null

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = P.foldr

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 = P.foldr1

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = P.foldl

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 = P.foldl1

maximum :: Ord a => [a] -> a
maximum = P.maximum

minimum :: Ord a => [a] -> a
minimum = P.minimum

all :: (a -> Bool) -> [a] -> Bool
all = P.all

any :: (a -> Bool) -> [a] -> Bool
any = P.any

and :: [Bool] -> Bool
and = P.and

or :: [Bool] -> Bool
or = P.or

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap = P.concatMap

notElem :: (Eq a) => a -> [a] -> Bool
notElem = P.notElem
