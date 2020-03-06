module Redefinitions where

-- Aquí redefinimos las funciones del prelude para adaptar sus tipos 

-- Listas
-- usamos [] en lugar de Foldable t => t

import Prelude (Bool, Show, Ord, Eq, Monad, Enum, (.))
import qualified Prelude as P
import Number

length :: [a] -> Number
length = integralToNumber . P.length

concat :: [[a]] -> [a]
concat = P.concat

elem :: Eq a => a -> [a] -> Bool
elem = P.elem

sum :: [Number] -> Number
sum = P.sum

product :: [Number] -> Number
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

sequence_ :: (Monad m) => [m a] -> m ()
sequence_ = P.sequence_

mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_ = P.mapM_

-- Redefiniciones de Num a Number

(+) :: Number -> Number -> Number
(+) = (P.+)

(*) :: Number -> Number -> Number
(*) = (P.*)

(-) :: Number -> Number -> Number
(-) = (P.-)

(/) :: Number -> Number -> Number
(/) = (P./)

div :: Number -> Number -> Number
div divisor dividendo = integralToNumber P.$
    P.div (numberToIntegral divisor) (numberToIntegral dividendo)

take cantidad = P.take (numberToIntegral cantidad)

lista !! posicion = lista P.!! (numberToIntegral posicion)

isFractional :: Number -> Bool
isFractional numero = P.floor numero P./= P.ceiling numero

abs :: Number -> Number
abs = P.abs

signum :: Number -> Number
signum = P.signum

negate :: Number -> Number
negate = P.negate

even :: Number -> Bool
even numero = P.even (numberToIntegral numero)

odd :: Number -> Bool
odd numero = P.odd (numberToIntegral numero)


