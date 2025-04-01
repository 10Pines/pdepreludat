{-# LANGUAGE FlexibleContexts #-}

module BetterErrors where

import Prelude
import GHC.TypeError

-- Para funciones

type ErrorNumeroXFuncion = Text "Estás usando una función como un número o un número como una función."
type ErrorOrdenableXFuncion = Text "Las funciones no se pueden ordenar ni comparar."
type ErrorEnumerableXFuncion = Text "Las funciones no son enumerables."

instance Unsatisfiable ErrorNumeroXFuncion => Num (a -> b)
instance Unsatisfiable ErrorOrdenableXFuncion => Ord (a -> b)
instance Unsatisfiable ErrorOrdenableXFuncion => Eq (a -> b)
instance Unsatisfiable ErrorNumeroXFuncion => Floating (a -> b)
instance Unsatisfiable ErrorNumeroXFuncion => Fractional (a -> b)
instance (Unsatisfiable ErrorNumeroXFuncion,
          Unsatisfiable ErrorEnumerableXFuncion,
          Unsatisfiable ErrorOrdenableXFuncion) => Integral (a -> b)
instance (Unsatisfiable ErrorNumeroXFuncion, Unsatisfiable ErrorOrdenableXFuncion) => RealFrac (a -> b)
instance (Unsatisfiable ErrorNumeroXFuncion, Unsatisfiable ErrorOrdenableXFuncion) => RealFloat (a -> b)
instance (Unsatisfiable ErrorNumeroXFuncion, Unsatisfiable ErrorOrdenableXFuncion) => Real (a -> b)
instance (Unsatisfiable ErrorEnumerableXFuncion) => Enum (a -> b)

-- Para listas
type ErrorNumeroXLista = Text "Estás usando una lista como un número o un número como una lista."
type ErrorEnumerableXLista = Text "Las listas no son enumerables."

instance Unsatisfiable ErrorNumeroXLista => Num [a]
instance Unsatisfiable ErrorNumeroXLista => Floating [a]
instance Unsatisfiable ErrorNumeroXLista => Fractional [a]
instance (Ord a, Unsatisfiable ErrorEnumerableXLista, Unsatisfiable ErrorNumeroXLista) => Integral [a]
instance (Ord a, Unsatisfiable ErrorNumeroXLista) => RealFrac [a]
instance (Ord a, Unsatisfiable ErrorNumeroXLista) => RealFloat [a]
instance (Ord a, Unsatisfiable ErrorNumeroXLista) => Real [a]
instance (Unsatisfiable ErrorNumeroXLista) => Enum [a]

-- Para caracteres
type ErrorNumeroXCaracter = Text "Estás usando una caracter como un número o un número como un caracter."

instance Unsatisfiable ErrorNumeroXCaracter => Num Char
instance Unsatisfiable ErrorNumeroXCaracter => Floating Char
instance Unsatisfiable ErrorNumeroXCaracter => Fractional Char
instance Unsatisfiable ErrorNumeroXCaracter => Integral Char
instance Unsatisfiable ErrorNumeroXCaracter => RealFrac Char
instance Unsatisfiable ErrorNumeroXCaracter => RealFloat Char
instance Unsatisfiable ErrorNumeroXCaracter => Real Char

-- Errores entre enteros y fraccionales
type ErrorFraccionalXEntero =
    Text "Estás usando un entero como un decimal o viceversa, y los números enteros y decimales son de diferente tipo.\nPodés convertir el entero en decimal usando toFloat/1 o el decimal en entero usando round/1, floor/1 o ceiling/1.\n\nTambién, si querés leer mas al respecto:\nhttp://wiki.uqbar.org/wiki/articles/problemas-comunes-con-los-tipos-numericos-de-haskell.html\n"

instance Unsatisfiable ErrorFraccionalXEntero => Fractional Int
instance Unsatisfiable ErrorFraccionalXEntero => Fractional Integer
instance Unsatisfiable ErrorFraccionalXEntero => Integral Float
instance Unsatisfiable ErrorFraccionalXEntero => Integral Double