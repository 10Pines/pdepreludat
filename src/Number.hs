module Number (Number,
               fromInteger,
               fromRational,
               fromReal,
               integralToNumber,
               numberToFractional,
               numberToIntegral,
               integerToNumber) where

import Prelude (($), (.), (<$>), (<>))
import qualified Prelude as P
import Data.Ratio (Ratio, Rational, (%), numerator, denominator)
import GHC.Real (Ratio(..), Real (toRational))
import GHC.Num (divInteger)
import Numeric (showFFloat)
import GHC.Stack (HasCallStack)
import Text.Read.Lex (numberToRational)

newtype Number = Number { wrappedNum :: WrappedNum }
    deriving (P.RealFrac, P.Num, P.Real, P.Fractional, P.Eq, P.Ord
    -- , P.Floating
    ) via WrappedNum

type WrappedNum = Rational

-- Funciones para convertir entre Number y los Num del Prelude

numberToIntegral :: HasCallStack => (P.Integral a) => Number -> a
numberToIntegral number = case rounded number of
    Integer integer -> P.fromInteger integer
    Decimal _ -> P.error $ "Se esperaba un valor entero pero se pasó uno con decimales: " P.++ P.show number

-- El numero pero redondeado y ya conociendo si es entero o decimal

data RoundedNumber = Integer P.Integer | Decimal WrappedNum

rounded :: Number -> RoundedNumber
rounded (Number number) | hasNoDecimalPart number = Decimal number
                        | P.otherwise = Integer $ P.floor number
    where hasNoDecimalPart = (P./= wrappedNum 0) . P.snd . P.properFraction

isInteger :: Number -> P.Bool
isInteger number = case rounded number of
    Integer _ -> P.True
    Decimal _ -> P.False

isDecimal :: Number -> P.Bool
isDecimal  = P.not . isInteger

numberToFractional :: (P.Fractional a) => Number -> a
numberToFractional = P.realToFrac

integralToNumber :: P.Integral a => a -> Number
integralToNumber number = P.fromIntegral number :: Number

integerToNumber :: P.Integer -> Number
integerToNumber number = P.fromInteger number :: Number

-- Redefiniciones para que los números literales se concreticen a Number por defecto

fromInteger :: P.Integer -> Number
fromInteger = P.fromInteger

fromRational :: P.Rational -> Number
fromRational = P.fromRational

fromReal :: Real a => a -> Number
fromReal = Number . toRational

withDouble :: (P.Double -> P.Double) -> (Number -> Number)
withDouble f = fromReal . f . numberToFractional 

instance P.Floating Number where
    pi = fromReal (P.pi :: P.Double)
    exp = withDouble P.exp
    log = withDouble P.log
    sin = withDouble P.sin
    cos = withDouble P.cos
    asin = withDouble P.asin
    acos = withDouble P.acos
    atan = withDouble P.atan
    sinh = withDouble P.sinh
    cosh = withDouble P.cosh
    asinh = withDouble P.asinh
    acosh = withDouble P.acosh
    atanh = withDouble P.atanh

instance P.Show Number where
    show (Number (numerator :% denominator))
        | decimalPart P.== decimalZero = integerPartAsString
        | isBetween0AndMinus1 = "-" <> integerPartAsString <> decimalPartAsString
        | P.otherwise = integerPartAsString <> decimalPartAsString

        where   integerPartAsString = P.show integerPart
                integerPart = numerator `P.quot` denominator :: P.Integer          

                decimalPartAsString = P.dropWhile (P./= '.') (showFFloat P.Nothing decimalPart "")
                remainder = numerator `P.rem` denominator
                decimalPart = P.fromInteger remainder P./ P.fromInteger denominator

                isBetween0AndMinus1 = integerPart P.== integerZero P.&& isNegative numerator
                isNegative n = n P.< integerZero
                integerZero = numberToIntegral 0
                decimalZero = numberToFractional 0


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
