module BetterErrors where
import Prelude
import GHC.TypeLits

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