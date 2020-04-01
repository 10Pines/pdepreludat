module Number where 

import Prelude (($))
import qualified Prelude as P

newtype Number = Number P.Double deriving (P.Show, P.Eq, P.Ord, P.Num, P.RealFrac, P.Real, P.Fractional, P.Enum, P.Floating) via P.Double

-- Funciones para convertir entre Number y los Num del Prelude

numberToIntegral :: (P.Integral a) => Number -> a
numberToIntegral n | isFractional n = P.error "Se esperaba un valor entero pero se pasó uno con decimales"
                   | P.otherwise = P.floor n
    where isFractional numero = P.floor numero P./= P.ceiling numero

numberToFractional :: (P.Fractional a) => Number -> a
numberToFractional = P.realToFrac

numberToFloat :: (P.RealFloat a) => Number -> a
numberToFloat = P.realToFrac

integralToNumber :: P.Integral a => a -> Number
integralToNumber number = P.fromIntegral number :: Number

integerToNumber :: P.Integer -> Number
integerToNumber number = P.fromInteger number :: Number

-- Redefiniciones para que los números literales se concreticen a Number por defecto

fromInteger :: P.Integer -> Number
fromInteger = P.fromInteger

fromRational :: P.Rational -> Number
fromRational = P.fromRational