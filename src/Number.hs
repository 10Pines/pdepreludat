module Number where 

import Prelude (($), (.))
import qualified Prelude as P

newtype Number = Number { wrappedNum :: WrappedNum }
    deriving (P.RealFrac, P.Num, P.Real, P.Fractional, P.Enum, P.Floating) via WrappedNum

type WrappedNum = P.Double

-- Funciones para convertir entre Number y los Num del Prelude

numberToIntegral :: (P.Integral a) => Number -> a
numberToIntegral (Number number) | isFractional roundedNumber = P.error "Se esperaba un valor entero pero se pasó uno con decimales"
                                 | P.otherwise = P.floor roundedNumber
    where isFractional numero = P.floor numero P./= P.ceiling numero
          roundedNumber = roundWrappedNum number

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

-- Redondeos para evitar los errores que pueden surgir de trabajar con numeros de punto flotante

roundWrappedNum :: WrappedNum -> WrappedNum
roundWrappedNum = roundingTo digitsAfterComma

digitsAfterComma :: P.Integer
digitsAfterComma = P.round $ wrappedNum 9.0

roundingTo :: P.Integer -> WrappedNum -> WrappedNum
roundingTo n = (P./ exp) . P.fromIntegral . P.round . (P.* exp)
    where exp = (numberToFractional 10) P.^ n

instance P.Ord Number where
    compare (Number a) (Number b) = P.compare (roundWrappedNum a) (roundWrappedNum b)

instance P.Eq Number where
    Number a == Number b = roundWrappedNum a P.== roundWrappedNum b

instance P.Show Number where
    show (Number x) = P.show $ roundWrappedNum x