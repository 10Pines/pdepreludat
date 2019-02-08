{-# LANGUAGE DataKinds, TypeOperators, UndecidableInstances #-}

module Lib
    ( someFunc
    ) where

import GHC.TypeLits

someFunc :: IO ()
someFunc = putStrLn "someFunc"

instance TypeError (Text "Cannot 'Show' functions." :$$: 
                    Text "Perhaps there is a missing argument?")
         => Show (a -> b) where
   showsPrec = error "unreachable"

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
