{-# LANGUAGE DataKinds, TypeOperators, UndecidableInstances, FlexibleInstances, ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import GHC.TypeLits
import Data.Typeable

someFunc :: IO ()
someFunc = putStrLn "someFunc"

instance Show (a -> b) where
    show _ = "<una función>"

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
