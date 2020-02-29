module PdePreludat (
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
    size,
    toFloat
) where 

import Prelude hiding (concat, length, elem, sum, product, null,
                       foldr, foldr1, foldl, foldl1, maximum, minimum,
                       all, any, and, or, concatMap, notElem)
import qualified Prelude as P
import GHC.TypeLits
import Data.Typeable

newtype Number = Number P.Double deriving (Show, Eq, Ord, Num, RealFrac, Real, Fractional) via P.Double

numberToIntegral :: (Integral a) => Number -> a
numberToIntegral = round

numberToFractional :: (Fractional a) => Number -> a
numberToFractional = realToFrac

integralToNumber :: Integral a => a -> Number
integralToNumber number = P.fromIntegral number :: Number

integerToNumber :: P.Integer -> Number
integerToNumber number = P.fromInteger number :: Number

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

-- TODO: intentar hacer funcionar esto para filter
-- instance (Typeable a, Typeable b) => Show (a -> b) where
--     show _ = "Una función de tipo: "
--         ++ show (typeOf (undefined :: a)) ++ " -> "
--         ++ show (typeOf (undefined :: b))

-- Custom Type Errors

--- Para funciones

type ErrorNumeroXFuncion = Text "Estás usando una función como un número o un número como una función."
type ErrorOrdenableXFuncion = Text "Las funciones no se pueden ordenar ni comparar."
type ErrorEnumerableXFuncion = Text "Las funciones no son enumerables."

instance TypeError (ErrorNumeroXFuncion) => Num (a -> b)
instance TypeError ErrorOrdenableXFuncion => Ord (a -> b)
instance TypeError ErrorOrdenableXFuncion => Eq (a -> b)
instance TypeError ErrorNumeroXFuncion => Floating (a -> b)
instance TypeError ErrorNumeroXFuncion => Fractional (a -> b)
instance (TypeError ErrorNumeroXFuncion,
          TypeError ErrorEnumerableXFuncion,
          TypeError ErrorOrdenableXFuncion) => Integral (a -> b)
instance (TypeError ErrorNumeroXFuncion, TypeError ErrorOrdenableXFuncion) => RealFrac (a -> b)
instance (TypeError ErrorNumeroXFuncion, TypeError ErrorOrdenableXFuncion) => RealFloat (a -> b)
instance (TypeError ErrorNumeroXFuncion, TypeError ErrorOrdenableXFuncion) => Real (a -> b)
instance (TypeError ErrorEnumerableXFuncion) => Enum (a -> b)
--instance Integral

---Para listas
type ErrorNumeroXLista = Text "Estás usando una lista como un número o un número como una lista."
type ErrorEnumerableXLista = Text "Las listas no son enumerables."

instance TypeError ErrorNumeroXLista => Num [a]
instance TypeError ErrorNumeroXLista => Floating [a]
instance TypeError ErrorNumeroXLista => Fractional [a]
instance (Ord a, TypeError ErrorEnumerableXLista, TypeError ErrorNumeroXLista) => Integral [a]
instance (Ord a, TypeError ErrorNumeroXLista) => RealFrac [a]
instance (Ord a, TypeError ErrorNumeroXLista) => RealFloat [a]
instance (Ord a, TypeError ErrorNumeroXLista) => Real [a]
instance (TypeError ErrorNumeroXLista) => Enum [a]

---Para caracteres
type ErrorNumeroXCaracter = Text "Estás usando una caracter como un número o un número como un caracter."

instance TypeError ErrorNumeroXCaracter => Num Char
instance TypeError ErrorNumeroXCaracter => Floating Char
instance TypeError ErrorNumeroXCaracter => Fractional Char
instance TypeError ErrorNumeroXCaracter => Integral Char
instance TypeError ErrorNumeroXCaracter => RealFrac Char
instance TypeError ErrorNumeroXCaracter => RealFloat Char
instance TypeError ErrorNumeroXCaracter => Real Char

--Errores entre enteros y fraccionales
type ErrorFraccionalXEntero =
    Text "Estás usando un entero como un decimal o viceversa, y los números enteros y decimales son de diferente tipo.\nPodés convertir el entero en decimal usando toFloat/1 o el decimal en entero usando round/1, floor/1 o ceiling/1.\n\nTambién, si querés leer mas al respecto:\nhttp://wiki.uqbar.org/wiki/articles/problemas-comunes-con-los-tipos-numericos-de-haskell.html\n"

instance TypeError ErrorFraccionalXEntero => Fractional Int
instance TypeError ErrorFraccionalXEntero => Fractional Integer
instance TypeError ErrorFraccionalXEntero => Integral Float
instance TypeError ErrorFraccionalXEntero => Integral Double

(...) :: a
(...) = error "Falta implementar."

implementame :: a
implementame = (...)

arreglame :: a
arreglame = (...)

size :: [a] -> Int
size = length

toFloat :: Integral a => a -> Float
toFloat = fromIntegral