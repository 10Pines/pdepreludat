{-# LANGUAGE DataKinds, TypeOperators, UndecidableInstances, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
module PdePreludat (
    (/),
    module Prelude,
    concat,
    length,
    elem,
    sum,
    product,
    null,
    foldr,
    foldr1,
    foldl,
    foldl1,
    maximum,
    minimum,
    all,
    any,
    and,
    or,
    concatMap,
    notElem,
    implementame,
    arreglame,
    (...),
    size
) where 

import Prelude hiding ((/), concat, length, elem, sum, product, null,
                       foldr, foldr1, foldl, foldl1, maximum, minimum,
                       all, any, and, or, concatMap, notElem)
import qualified Prelude as P
import GHC.TypeLits
import Data.Typeable

-- Reemplazos para Foldable
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

-- Show de funciones
instance Show (a -> b) where
    show _ = "<una función>"

class Fractionable a where
    toFractional :: (Fractional b) => a -> b

instance Fractionable Int where
    toFractional entero = fromIntegral entero

instance Fractionable Integer where
    toFractional entero = fromIntegral entero

instance Fractionable Double where
    toFractional = fromRational . toRational

instance Fractionable Float where
    toFractional = fromRational . toRational

(/) :: (Fractionable a, Fractionable b, Fractional c) => a -> b -> c
a / b = toFractional a P./ toFractional b

-- TODO: intentar hacer funcionar esto para filter
-- instance (Typeable a, Typeable b) => Show (a -> b) where
--     show _ = "Una función de tipo: "
--         ++ show (typeOf (undefined :: a)) ++ " -> "
--         ++ show (typeOf (undefined :: b))

instance TypeError (Text "Estás usando una función como un número") => Num (a -> b) where
    (+) = error "unreacheable"
    (*) = error "unreacheable"
    abs = error "unreacheable"
    signum = error "unreacheable"
    fromInteger = error "unreacheable"
    negate = error "unreacheable"
instance TypeError (Text "Estás operando una lista con un número") => Num [a] where
    (+) = error "unreacheable"
    (*) = error "unreacheable"
    abs = error "unreacheable"
    signum = error "unreacheable"
    fromInteger = error "unreacheable"
    negate = error "unreacheable"

(...) :: a
(...) = error "Falta implementar."

implementame :: a
implementame = (...)

arreglame :: a
arreglame = (...)

size :: [a] -> Int
size = length
