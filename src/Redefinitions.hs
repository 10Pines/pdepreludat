module Redefinitions where

-- Aquí redefinimos las funciones del prelude para adaptar sus tipos 

-- Listas
-- usamos [] en lugar de Foldable t => t

import Prelude (Bool, Show, Ord, Eq, Monad, Enum, (.), (==))
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

(^) :: Number -> Number -> Number
(^) numero exponente = (P.^) (numberToFractional numero) (numberToIntegral exponente)

(^^) :: Number -> Number -> Number
(^^) numero exponente = (P.^^) (numberToFractional numero) (numberToIntegral exponente)

subtract :: Number -> Number -> Number
subtract = P.subtract

(/) :: Number -> Number -> Number
_ / 0 = P.error "División por cero"
numerador / divisor = numerador P./ divisor

div :: Number -> Number -> Number
div divisor dividendo = integralToNumber P.$
    P.div (numberToIntegral divisor) (numberToIntegral dividendo)

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

lcm :: Number -> Number -> Number
lcm numero1 numero2 = integralToNumber(P.lcm (numberToIntegral numero1) (numberToIntegral numero2))

gcd :: Number -> Number -> Number
gcd numero1 numero2 = integralToNumber(P.gcd (numberToIntegral numero1) (numberToIntegral numero2))

ceiling :: Number -> Number
ceiling numero = integralToNumber (P.ceiling numero)

floor :: Number -> Number
floor numero = integralToNumber (P.floor numero)

round :: Number -> Number
round numero = integralToNumber (P.round numero)

truncate :: Number -> Number
truncate numero = integralToNumber (P.truncate numero)

-- Redefiniciones de Números y listas

take cantidad = P.take (numberToIntegral cantidad)

lista !! posicion = lista P.!! (numberToIntegral posicion)

drop :: Number -> [a] -> [a]
drop cantidad = P.drop (numberToIntegral cantidad)

replicate :: Number -> a -> [a]
replicate veces = P.replicate (numberToIntegral veces)

splitAt :: Number -> [a] -> ([a], [a])
splitAt posicion = P.splitAt (numberToIntegral posicion)

quot :: Number -> Number -> Number
quot unNumero otroNumero =
    integralToNumber P.$ P.quot (numberToIntegral unNumero) (numberToIntegral otroNumero)

rem :: Number -> Number -> Number
rem unNumero otroNumero =
    integralToNumber P.$ P.rem (numberToIntegral unNumero) (numberToIntegral otroNumero)

mod :: Number -> Number -> Number
mod unNumero otroNumero =
    integralToNumber P.$ P.mod (numberToIntegral unNumero) (numberToIntegral otroNumero)

quotRem :: Number -> Number -> (Number, Number)
quotRem unNumero otroNumero =
    case P.quotRem (numberToIntegral unNumero) (numberToIntegral otroNumero) of
        (quot, rem) -> (integralToNumber quot, integralToNumber rem)

divMod :: Number -> Number -> (Number, Number)
divMod unNumero otroNumero =
    case P.divMod (numberToIntegral unNumero) (numberToIntegral otroNumero) of
        (div, mod) -> (integralToNumber div, integralToNumber mod)

pi :: Number
pi = P.pi

exp :: Number -> Number
exp = P.exp

log :: Number -> Number
log = P.log

sqrt :: Number -> Number
sqrt = P.sqrt

(**) :: Number -> Number -> Number
(**) = (P.**)

logBase :: Number -> Number -> Number
logBase = P.logBase

sin :: Number -> Number
sin = P.sin

cos :: Number -> Number
cos = P.cos

tan :: Number -> Number
tan = P.tan

asin :: Number -> Number
asin = P.asin

acos :: Number -> Number
acos = P.acos

atan :: Number -> Number
atan = P.atan

sinh :: Number -> Number
sinh = P.sinh

cosh :: Number -> Number
cosh = P.cosh

tanh :: Number -> Number
tanh = P.tanh

asinh :: Number -> Number
asinh = P.asinh

acosh :: Number -> Number
acosh = P.acosh

atanh :: Number -> Number
atanh = P.atanh
