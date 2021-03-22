module Redefinitions where

-- Aquí redefinimos las funciones del prelude para adaptar sus tipos 

-- Listas
-- usamos [] en lugar de Foldable t => t

import Prelude (Bool, Show, Ord, Eq, Monad, Enum, (.), (==))
import qualified Prelude as P
import Number

-- | Recibe una lista y devuelve la cantidad de elementos en la misma.
--
-- >>> length []
-- 0
-- >>> length [1,2,3]
-- 3
length :: [a] -> Number
length = integralToNumber . P.length

-- | Recibe una lista de listas y devuelve la concatenación de todas esas listas.
--
-- >>> concat [[1,2,3], [4,5,6]]
-- [1,2,3,4,5,6]
-- >>> concat ["hola", " ", "mundo"]
-- "hola mundo"
concat :: [[a]] -> [a]
concat = P.concat

-- | Recibe un valor de algún tipo equiparable, una lista con elementos de ese tipo
-- y me dice si el elemento está en la lista.
--
-- >>> elem "hola" ["hola", "chau"]
-- True
-- >>> elem 1 [0, 2, 4]
-- False
-- >>> elem 1 []
-- False
elem :: Eq a => a -> [a] -> Bool
elem = P.elem

-- | Recibe una lista de números y retorna la sumatoria de los mismos.
-- Si la lista esta vacía devuelve 0.
--
-- >>> sum []
-- 0
-- >>> sum [33, 12]
-- 45
sum :: [Number] -> Number
sum = P.sum

-- | Recibe una lista de números y retorna el producto de los mismos.
-- Si la lista esta vacía devuelve 1.
--
-- >>> product []
-- 1
-- >>> product [2, 3]
-- 6
product :: [Number] -> Number
product = P.product

-- | Indica si una lista está vacía (no tiene elementos).
--
-- >>> null []
-- True
-- >>> null [1, 2, 3]
-- False
null ::  [a] -> Bool
null = P.null

-- | Recibe una función de dos parámetros, una semilla y una lista.
--
-- Una forma de entender el fold es pensando que aplica la función con la semilla
-- y un elemento de la lista, luego aplica la función con el resultado obtenido
-- y con el siguiente elemento de la lista, y así hasta que no queden elementos en la lista.
--
-- Otra forma de pensarlo es que dada una lista, pone la función entre todos los elementos:
--
-- foldr (+) 0 [1, 2, 3, 4, 5]
--
-- 1 + 2 + 3 + 4 + 5 
--
-- Agrega la semilla a la derecha
--
-- 1 + 2 + 3 + 4 + 5 + 0
--
-- Y pone paréntesis agrupando a la derecha (por eso la r de right)
--
-- 1 + (2 + (3 + (4 + (5 + 0))))
--
-- Si queres leer más sobre esta familia de funciones, podes consultar acá:
-- https://wiki.uqbar.org/wiki/articles/fold.html
--
-- __OJO:__ ¡recordá que el tipo de retorno del fold es del mismo tipo que la semilla!
--
-- >>> foldr (+) 0 [1, 2, 3, 4, 5]
-- 13
-- >>> foldr (++) "" ["hola", " ", "mundo"]
-- "hola mundo"
-- >>> foldr max 5 [-1, 3, 10, 0]
-- 10
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = P.foldr

-- | Similar a 'foldr' pero no recibe una semilla como parámetro
--
-- Recibe una función que toma dos elementos del mismo tipo y me devuelve otro, una lista de elementos de ese tipo y
-- devuelve el resultado de aplicar la función sucesivamente sobre elementos de la lista.
--
-- __NOTA:__ si se usa con una lista vacía, falla.
--
-- >>> foldr1 (+) [1,2,3]
-- 6
-- >>> foldr1 (++) ["abre", "latas"]
-- "abrelatas"
-- >>> foldr1 min ["arco", "arbol", "zapallo"]
-- "arbol"
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 = P.foldr1

-- | Recibe una función de dos parámetros, una semilla y una lista.
--
-- Una forma de entender el fold es pensando que aplica la función con la semilla
-- y un elemento de la lista, luego aplica la función con el resultado obtenido
-- y con el siguiente elemento de la lista, y así hasta que no queden elementos en la lista.
--
-- Otra forma de pensarlo es que dada una lista, pone la función entre todos los elementos:
--
-- foldl (+) 0 [1, 2, 3, 4, 5]
--
-- 1 + 2 + 3 + 4 + 5 
--
-- Agrega la semilla a la izquierda
--
-- 0 + 1 + 2 + 3 + 4 + 5
--
-- Y pone paréntesis agrupando a la izquierda (por eso la l de left)
--
-- ((((0 + 1) + 2) + 3) + 4) + 5
--
-- Si queres leer más sobre esta familia de funciones, podes consultar acá:
-- https://wiki.uqbar.org/wiki/articles/fold.html
--
-- __OJO:__ ¡recordá que el tipo de retorno del fold es del mismo tipo que la semilla!
--
-- >>> foldl (+) 0 [1, 2, 3, 4, 5]
-- 13
-- >>> foldl (++) "" ["hola", " ", "mundo"]
-- "hola mundo"
-- >>> foldl max 5 [-1, 3, 10, 0]
-- 10
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = P.foldl

-- | Similar a 'foldl' pero no recibe una semilla como parámetro
--
-- Recibe una función que toma dos elementos del mismo tipo y me devuelve otro, una lista de elementos de ese tipo y
-- devuelve el resultado de aplicar la función sucesivamente sobre elementos de la lista.
--
-- __NOTA:__ si se usa con una lista vacía, falla.
--
-- >>> foldl1 (+) [1,2,3]
-- 6
-- >>> foldl1 (++) ["abre", "latas"]
-- "abrelatas"
-- >>> foldl1 min ["arco", "arbol", "zapallo"]
-- "arbol"
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 = P.foldl1

-- | Recibe una lista de valores ordenables y devuelve el máximo de la lista.
-- 
-- Si la lista está vacía falla.
--
-- >>> maximum [1, 9, 5]
-- 9
-- >>> maximum ["arco", "arbol", "zapallo"]
-- "zapallo"
maximum :: Ord a => [a] -> a
maximum = P.maximum

-- | Recibe una lista de valores ordenables y devuelve el máximo de la lista.
-- 
-- Si la lista está vacía falla.
--
-- >>> minimum [1, 9, 5]
-- 1
-- >>> minimum ["arco", "arbol", "zapallo"]
-- "arbol"
minimum :: Ord a => [a] -> a
minimum = P.minimum

-- | Recibe una condición (una función que recibe un parámetro y devuelve Bool), una lista
-- y retorna True si todos los elementos de la lista cumplen la condición.
--
-- >>> all even [2, 4, 6]
-- True
-- >>> all even [2, 4, 7]
-- False
all :: (a -> Bool) -> [a] -> Bool
all = P.all

-- | Recibe una condición (una función que recibe un parámetro y devuelve Bool), una lista
-- y retorna True si al menos un elemento de la lista cumple la condición.
--
-- >>> any even [2, 3, 5]
-- True
-- >>> any even [1, 3, 5]
-- False
any :: (a -> Bool) -> [a] -> Bool
any = P.any

-- | Recibe una lista de booleanos y retorna True si todos los elementos son True y False si alguno es False.
--
-- >>> and [True, True]
-- True
-- >>> and [False, True]
-- False
and :: [Bool] -> Bool
and = P.and

-- | Recibe una lista de booleanos y retorna True si al menos un elemento es True y False si todos son False.
--
-- >>> or [False, True]
-- True
-- >>> or [False, False]
-- False
or :: [Bool] -> Bool
or = P.or

-- | Recibe una lista y una función de un parámetro que devuelve una lista. Aplica la función a cada elemento
-- de la lista generando una lista de listas, y luego concatena esas listas.
--
-- >>> concatMap (\x -> [x, x]) [1, 2, 3]
-- [1, 1, 2, 2, 3, 3]
-- >>> concatMap (\(nombre, notas) -> notas) [("feche", [9, 10, 8]), ("aye", [8, 9, 10])]
-- [9, 10, 8, 8, 9, 10]
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap = P.concatMap

-- | El opuesto de 'elem'
-- 
-- Recibe un valor de algún tipo equiparable, una lista con elementos de ese tipo
-- y me dice si el elemento __no__ está en la lista.
--
-- >>> elem "hola" ["hola", "chau"]
-- False
-- >>> elem 1 [0, 2, 4]
-- True
-- >>> elem 1 []
-- True
notElem :: (Eq a) => a -> [a] -> Bool
notElem = P.notElem

-- | Recibe una lista de mónadas y evalúa cada valor de izquierda a derecha ignorando los resultados
-- Para una versión que no ignora los resultados, mirar 'sequence'.
--
-- >>> sequence_ [print 5, print 10]
-- 5
-- 10 -- Esto NO es el valor de retorno, 5 y 10 se imprimen por pantalla. El valor de retorno es (), o sea Unit, y ghci decide no mostrarlo
sequence_ :: (Monad m) => [m a] -> m ()
sequence_ = P.sequence_

-- | Recibe una función que recibe un parámetro y devuelve una mónada, y una lista. Aplica la función a cada elemento,
-- evalúa las mónadas resultantes según el orden de la lista e ignora los resultados.
-- Para una versión que no ignora los resultados, mirar 'mapM'.
--
-- >>> mapM_ print [5, 10]
-- 5
-- 10 -- Esto NO es el valor de retorno, 5 y 10 se imprimen por pantalla. El valor de retorno es (), o sea Unit, y ghci decide no mostrarlo
mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_ = P.mapM_

-- Redefiniciones de Num a Number

infixl 7  *
infixl 6  +, -
infixr 8  ^, ^^
infixl 7  /, `quot`, `rem`, `div`, `mod`

-- | La suma de dos números.
--
-- >>> 2 + 2
-- 4
-- >>> 4 + 2
-- 6
(+) :: Number -> Number -> Number
(+) = (P.+)

-- | El producto de dos números.
--
-- >>> 5 * 5
-- 25
(*) :: Number -> Number -> Number
(*) = (P.*)

-- | La resta de dos números.
--
-- >>> 20 - 10
-- 10
-- 
-- __OJO:__ en haskell, -5 puede significar una función que resta 5 o el número 5 negativo dependiendo del contexto.
-- Por ejemplo, si intentás hacer:
--
-- >>> 10 -5
--
-- Vas a obtener 5, pero si hacés:
--
-- >>> map (-5) [1,2,3]
--
-- Haskell te va a tirar un error porque lo está interpretando como el número, no la función. En estos casos podés
-- usar 'subtract' en vez de '-', así:
--
-- >>> map (subtract 5) [1,2,3]
-- [-4, -3, -2]
(-) :: Number -> Number -> Number
(-) = (P.-)

-- | Recibe primero una base y luego el exponente y retorna la base elevada al exponente
--
-- No soporta exponentes negativos, para eso se puede usar '^^'
-- >>> 2 ^ 3
-- 8
(^) :: Number -> Number -> Number
(^) numero exponente = (P.^) (numberToFractional numero) (numberToIntegral exponente)

-- | Recibe primero una base y luego el exponente y retorna la base elevada al exponente
--
-- A diferencia de '^', '^^' soporta exponentes negativos
--
-- >>> 2 ^^ 3
-- 8
-- >>> 2 ^^ (-3)
-- 0.125
(^^) :: Number -> Number -> Number
(^^) numero exponente = (P.^^) (numberToFractional numero) (numberToIntegral exponente)

-- | La resta de dos números. Recibe primero el sustraendo y luego el minuendo.
--
-- Es decir que substract 5 2 se lee como 2 - 5.
--
-- >>> subtract 5 2
-- -3
--
-- Es útil cuando se quiere aplicar una resta parcialmente, por ejemplo:
-- 
-- >>> map (subtract 10) [10, 20, 30]
-- [0, 10, 20]
subtract :: Number -> Number -> Number
subtract = P.subtract

-- | División con decimales.
--
-- >>> 5 / 2
-- 2.5
-- >>> 20 / 5
-- 4
(/) :: Number -> Number -> Number
_ / 0 = P.error "División por cero"
numerador / divisor = numerador P./ divisor

-- | División entera.
--
-- Utilizando notación infija
-- >>> 5 `div` 2
-- 2
--
-- Utilizando notación prefija
-- >>> div 20 5
-- 4
div :: Number -> Number -> Number
div divisor dividendo = integralToNumber P.$
    P.div (numberToIntegral divisor) (numberToIntegral dividendo)

-- | Indica si un número tiene decimales.
-- 
-- >>> isFractional 4
-- False
-- >>> isFractional 4.1
-- True
isFractional :: Number -> Bool
isFractional numero = P.floor numero P./= P.ceiling numero


-- | Devuelve el valor absoluto de un número.
-- 
-- >>> abs 2
-- 2
-- >>> abs (-2)
-- 2
abs :: Number -> Number
abs = P.abs

-- | Devuelve el signo de un número: 1 si es positivo, -1 si es negativo.
-- 
-- >>> signum 29
-- 1
-- >>> signum (-29)
-- -1
signum :: Number -> Number
signum = P.signum

-- | Cambia el signo de un número, si es positivo lo transforma a negativo y viceversa.
-- 
-- >>> negate 3
-- -3
-- >>> negate (-3)
-- 3
negate :: Number -> Number
negate = P.negate

-- | Indica si un número es par.
-- 
-- >>> even 7
-- False
-- >>> even 8
-- True
even :: Number -> Bool
even numero = P.even (numberToIntegral numero)

-- | Indica si un número es impar.
-- 
-- >>> odd 8
-- False
-- >>> odd 7
-- True
odd :: Number -> Bool
odd numero = P.odd (numberToIntegral numero)

-- | Dados dos números devuelve el mínimo común múltiplo.
-- 
-- Utilizando notación prefija
-- >>> lcm 5 6
-- 30
-- 
-- Utilizando notación infija
-- >>> 6 `lcm` 4
-- 12
lcm :: Number -> Number -> Number
lcm numero1 numero2 = integralToNumber(P.lcm (numberToIntegral numero1) (numberToIntegral numero2))

-- | Dados dos números devuelve el máximo común divisor.
-- 
-- Utilizando notación prefija
-- >>> gcd 5 6
-- 1
-- 
-- Utilizando notación infija
-- >>> 36 `gcd` 30
-- 6
gcd :: Number -> Number -> Number
gcd numero1 numero2 = integralToNumber(P.gcd (numberToIntegral numero1) (numberToIntegral numero2))

-- | Dado un número, devuelve el entero más cercano mayor a ese número.
-- 
-- >>> ceiling 5.1
-- 6
-- >>> ceiling 5
-- 5
ceiling :: Number -> Number
ceiling numero = integralToNumber (P.ceiling numero)

-- | Dado un número, devuelve el entero más cercano menor a ese número.
-- 
-- >>> floor 4.5
-- 4
-- >>> floor 4
-- 4
floor :: Number -> Number
floor numero = integralToNumber (P.floor numero)

-- | Dado un número, lo redondea a entero hacia arriba o abajo dependiendo de los decimales 
-- (hasta 0.5 exclusive redondea hacia abajo, a partir de 0.5 redondea hacia arriba).
-- 
-- >>> round 5.5
-- 6
-- >>> round 5.9
-- 6
-- >>> round 5.1
-- 5
round :: Number -> Number
round numero = integralToNumber (P.round numero)

-- | Dado un número, le saca los decimales.
-- 
-- >>> truncate 5.9
-- 5
-- >>> truncate 5
-- 5
truncate :: Number -> Number
truncate numero = integralToNumber (P.truncate numero)

-- Redefiniciones de Números y listas

-- | Elimina los primeros n elementos de una lista.
-- 
-- >>> take 4 [1..]
-- [1, 2, 3, 4]
-- >>> take 2 ["pasame", "el", "jabon", "no", "radio"]
-- ["pasame","el"]
-- >>> take -2 [5..8]
-- []
-- >>> take 3 []
-- []
take cantidad = P.take (numberToIntegral cantidad)

-- | Devuelve el enésimo elemento de una lista, donde la primera posición ocupa el índice 0, 
-- la segunda el índice 1, etc.
-- 
-- Utilizando notación prefija
-- >>> (!!) [1..5] 2
-- 3
-- Utilizando notación infija
-- >>> [1..5] !! 1
-- 2
lista !! posicion = lista P.!! (numberToIntegral posicion)


-- | Devuelve los últimos n elementos
-- la segunda el índice 1, etc.
-- 
-- Utilizando notación prefija
-- >>> (!!) [1..5] 2
-- 3
-- Utilizando notación infija
-- >>> [1..5] !! 1
-- 2
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
