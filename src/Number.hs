module Number (Number,
               fromInteger,
               fromRational,
               integralToNumber,
               numberToFractional,
               numberToIntegral,
               integerToNumber,
               numberToFloat) where 

import Prelude (($), (.), (<$>))
import qualified Prelude as P

newtype Number = Number { wrappedNum :: WrappedNum }
    deriving (P.RealFrac, P.Num, P.Real, P.Fractional, P.Floating) via WrappedNum

type WrappedNum = P.Double

-- Funciones para convertir entre Number y los Num del Prelude

numberToIntegral :: (P.Integral a) => Number -> a
numberToIntegral number = case rounded number of
    Integer integer -> P.fromInteger integer
    Decimal _ -> P.error $ "Se esperaba un valor entero pero se pasó uno con decimales: " P.++ P.show number

-- El numero pero redondeado y ya conociendo si es entero o decimal

data RoundedNumber = Integer P.Integer | Decimal WrappedNum

rounded :: Number -> RoundedNumber
rounded (Number number) | isFractional = Decimal roundedNumber
                        | P.otherwise = Integer $ P.floor roundedNumber
    where isFractional = P.floor roundedNumber P./= P.ceiling roundedNumber
          roundedNumber = roundWrappedNum number

isInteger :: Number -> P.Bool
isInteger number = case rounded number of
    Integer _ -> P.True
    Decimal _ -> P.False

isDecimal :: Number -> P.Bool
isDecimal  = P.not . isInteger

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
    show number = case rounded number of
        Integer integer -> P.show integer
        Decimal decimal -> P.show decimal

instance P.Enum Number where
    toEnum integer = Number $ P.toEnum integer
    fromEnum (Number n) = P.fromEnum n
    enumFromThenTo first second last = case P.all isInteger [first, second, last] of
        P.True -> fromInteger <$> P.enumFromThenTo
                                        (numberToIntegral first)
                                        (numberToIntegral second)
                                        (numberToIntegral last)
        P.False -> fromRational <$> P.enumFromThenTo
                                        (numberToFractional first)
                                        (numberToFractional second)
                                        (numberToFractional last)
