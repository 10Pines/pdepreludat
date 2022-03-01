module PdePreludat (
    module Prelude,
    module Redefinitions,
    module Number,
    implementame,
    arreglame,
    (...),
    ifThenElse,
) where

-- Estos modulos solo exportan instancias de typeclasses, que se exportan por default asi que no es necesario reexportarlos
import BetterErrors
import ShowFunction

-- Estos modulos definen funciones y tipos, así que es necesario reexportarlos.
import Number
import Redefinitions

-- El Prelude se importan solo aquellas cosas que vayan a ser reexportadas,
-- lo demás se define en los modulos del PdePreludat.
import Prelude (Ord(..), Eq(..), Show(..), Enum(..), Bool(..), (&&), Bounded(..), Char, Either(..), FilePath, IO, Functor(..), Applicative(..), Monad(..), IOError, Maybe(..), Semigroup(..), Monoid(..), Ordering(..), Read(..), ReadS(..), ShowS(..), String, Traversable(..), Word, appendFile, asTypeOf, break, const, curry, cycle, dropWhile, either, error, getChar, getContents, getLine, interact, ioError, iterate, lex, lines, lookup, maybe, print, putChar, putStr, putStrLn, read, readFile, readIO, readLn, readParen, reads, repeat, reverse, scanl, scanl1, scanr, scanr1, seq, showChar, showParen, showString, shows, span, takeWhile, uncurry, undefined, unlines, until, unwords, unzip, unzip3, userError, words, writeFile, zip3, zipWith3, (||), otherwise)

-- Este modulo no se reexporta, solo se importo para definir funciones en base a las definidas ahí.
import qualified Prelude as P

-- RebindableSyntax hace necesario que tengamos que definir ifThenElse si queremos
-- usar el patron if condicion then algo else otraCosa

ifThenElse :: Bool -> a -> a -> a
ifThenElse condition ifTrue ifFalse = case condition of
  True -> ifTrue
  False -> ifFalse

-- Valor pensado para usarla como implementación por defecto de funciones
-- o valores que pidamos que sean implementados en los ejercicios

-- | Se puede usar en vez de una expresión que aun no se implementó.
-- Tipa en cualquier lugar que se use pero al evaluarlo falla con un error de "Falta implementar."
(...) :: a
(...) = P.error "Falta implementar."

-- Aliases utiles

-- | Se puede usar en vez de una expresión que aun no se implementó.
-- Tipa en cualquier lugar que se use pero al evaluarlo falla con un error de "Falta implementar."
-- Alias de (...)
implementame :: a
implementame = (...)

-- | Se puede usar en vez de una expresión que aun no se implementó.
-- Tipa en cualquier lugar que se use pero al evaluarlo falla con un error de "Falta implementar."
-- Alias de (...)
arreglame :: a
arreglame = (...)

-- size :: [a] -> Number
-- size = length
