module Number (Number,
               fromInteger,
               fromRational,
               integralToNumber,
               numberToFractional,
               numberToIntegral,
               integerToNumber,
               numberToFloat) where

import Data.Bifunctor
import Data.Ratio
import Data.Either
import Data.Bifoldable

import Prelude (($), (.), (<$>))
import qualified Prelude as P

newtype Number = Number { wrappedNum :: WrappedNum }
    deriving (P.RealFrac, P.Num, P.Real, P.Fractional, P.Floating) via WrappedNum

type WrappedNum = Either P.Integer P.Double

-- Instancias de typeclasses numéricas

instance P.Num WrappedNum where
    Left x  + Left y  = Left (x P.+ y)
    Right x + Right y = toWrapped (x P.+ y)
    Left x  + Right y = toWrapped (P.fromIntegral x P.+ y)
    Right x + Left y  = toWrapped (x P.+ P.fromIntegral y)

    Left x  * Left y  = Left (x P.* y)
    Right x * Right y = toWrapped (x P.* y)
    Left x  * Right y = toWrapped (P.fromIntegral x P.* y)
    Right x * Left y  = toWrapped (x P.* P.fromIntegral y)

    abs = bimap P.abs P.abs
    signum = bimap P.signum P.signum
    fromInteger = Left
    negate = bimap P.negate P.negate

instance P.Real WrappedNum where
    toRational = P.either P.toRational P.toRational

instance P.Fractional WrappedNum where
    fromRational x = case (P.== numberToIntegral 1) . denominator $ x of
        P.True -> Left (numerator x)
        P.False -> case convertIfPossible . P.fromRational $ x of
            P.Just y -> Left y
            P.Nothing -> Right $ P.fromRational x

    recip = toWrapped . (numberToFloat 1 P./) . bifoldr (\x y -> y P.+ P.fromIntegral x) (P.+) (numberToFloat 0)

instance P.RealFrac WrappedNum where
    properFraction = either
        (\x -> (P.fromIntegral x, Left (numberToIntegral 0)))
        (second Right . P.properFraction)

instance P.Floating WrappedNum where
    pi = Right P.pi
    exp = toWrapped . P.exp . toDouble
    log = toWrapped . P.log . toDouble
    sin = toWrapped . P.sin . toDouble
    cos = toWrapped . P.cos . toDouble
    asin = toWrapped . P.asin . toDouble
    acos = toWrapped . P.acos . toDouble
    atan = toWrapped . P.atan . toDouble
    sinh = toWrapped . P.sinh . toDouble
    cosh = toWrapped . P.cosh . toDouble
    asinh = toWrapped . P.asinh . toDouble
    acosh = toWrapped . P.acosh . toDouble
    atanh = toWrapped . P.atanh . toDouble

toDouble :: WrappedNum -> P.Double
toDouble = either P.fromIntegral P.id

toWrapped :: P.Double -> WrappedNum
toWrapped x = case convertIfPossible x of
    P.Just y -> Left y
    P.Nothing -> Right . roundToWrap' $ x

-- Funciones para convertir entre Number y los Num del Prelude

numberToIntegral :: (P.Integral a) => Number -> a
numberToIntegral (Number number) = case number of
    Left integer -> P.fromInteger integer
    Right double -> case convertIfPossible double of
        P.Just y -> y
        P.Nothing -> P.error $ "Se esperaba un valor entero pero se pasó uno con decimales: " P.++ P.show double

convertIfPossible :: P.Integral a => P.Double -> P.Maybe a
convertIfPossible x = case (P.== numberToFloat 0) . P.snd . P.properFraction . roundToWrap $ x of
    P.True -> P.Just $ P.round x
    P.False -> P.Nothing

-- El numero pero redondeado y ya conociendo si es entero o decimal

isInteger :: Number -> P.Bool
isInteger = isLeft . wrappedNum

isDecimal :: Number -> P.Bool
isDecimal = P.not . isInteger

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

roundToWrap :: P.Double -> P.Double
roundToWrap = roundingTo digitsToCheckIfInteger

roundToWrap' :: P.Double -> P.Double
roundToWrap' = roundingTo digitsAfterComma

digitsToCheckIfInteger :: P.Integer
digitsToCheckIfInteger = numberToIntegral 9

digitsAfterComma :: P.Integer
digitsAfterComma = numberToIntegral 10

roundingTo :: P.Integer -> P.Double -> P.Double
roundingTo n = (P./ exp) . P.fromIntegral . P.round . (P.* exp)
    where exp = (numberToFractional 10) P.^ n

instance P.Ord Number where
    compare (Number (Left a))  (Number (Left b))  = P.compare a b
    compare (Number (Right a)) (Number (Right b)) = P.compare a b
    compare (Number (Left a))  (Number (Right b)) = P.compare (P.fromIntegral a) b
    compare (Number (Right a)) (Number (Left b))  = P.compare a (P.fromIntegral b)

instance P.Eq Number where
    Number a == Number b = a P.== b

instance P.Show Number where
    show = either P.show P.show . wrappedNum

instance P.Enum Number where
    toEnum integer = Number $ Left $ P.toInteger integer
    fromEnum (Number (Left n))  = P.fromEnum n
    fromEnum (Number (Right n)) = P.error $ "Se esperaba un valor entero pero se pasó uno con decimales: " P.++ P.show n

    enumFromThenTo first second last = case P.all isInteger [first, second, last] of
        P.True -> fromInteger <$> P.enumFromThenTo
                                        (numberToIntegral first)
                                        (numberToIntegral second)
                                        (numberToIntegral last)
        P.False -> fromRational <$> P.enumFromThenTo
                                        (numberToFractional first)
                                        (numberToFractional second)
                                        (numberToFractional last)
