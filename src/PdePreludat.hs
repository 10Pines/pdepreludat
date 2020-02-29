module PdePreludat (
    module Prelude,
    module FoldableByListReplacements,
    module Number,
    implementame,
    arreglame,
    (...),
) where

-- Estos modulos solo exportan instancias de typeclasses, que se exportan por default asi que no es necesario reexportarlos
import BetterErrors
import ShowFunction

-- Estos modulos definen funciones y tipos, así que es necesario reexportarlos.
import Number
import FoldableByListReplacements

-- El Prelude se importan solo aquellas cosas que vayan a ser reexportadas,
-- lo demás se define en los modulos del PdePreludat.
import Prelude (Ord(..), Eq(..), Show(..), Bool(..), ($), (&&), (++), (.), Bounded(..), Char, Either(..), Enum(..), FilePath, IO, Functor(..), Applicative(..), Monad(..), IOError, Maybe(..), Semigroup(..), Monoid(..), Ordering(..), Read(..), ReadS(..), ShowS(..), String, Traversable(..), Word, appendFile, asTypeOf, break, const, curry, cycle, dropWhile, either, error, filter, flip, fst, getChar, getContents, getLine, head, id, init, interact, ioError, iterate, last, lex, lines, lookup, map, maybe, not, otherwise, print, putChar, putStr, putStrLn, read, readFile, readIO, readLn, readParen, reads, repeat, reverse, scanl, scanl1, scanr, scanr1, seq, showChar, showParen, showString, shows, snd, span, tail, takeWhile, uncurry, undefined, unlines, until, unwords, unzip, unzip3, userError, words, writeFile, zip, zip3, zipWith, zipWith3, (||))

-- Este modulo no se reexporta, solo se importo para definir funciones en base a las definidas ahí.
import qualified Prelude as P

(...) :: a
(...) = P.error "Falta implementar."

-- Aliases utiles

implementame :: a
implementame = (...)

arreglame :: a
arreglame = (...)

-- size :: [a] -> Number
-- size = length
