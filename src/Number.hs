module Number where 

import Prelude (($))
import qualified Prelude as P

newtype Number = Number P.Double deriving (P.Show, P.Eq, P.Ord, P.Num, P.RealFrac, P.Real, P.Fractional) via P.Double

numberToIntegral :: (P.Integral a) => Number -> a
numberToIntegral = P.round

numberToFractional :: (P.Fractional a) => Number -> a
numberToFractional = P.realToFrac

integralToNumber :: P.Integral a => a -> Number
integralToNumber number = P.fromIntegral number :: Number

integerToNumber :: P.Integer -> Number
integerToNumber number = P.fromInteger number :: Number

-- Redefiniciones de Num a Number

(+) :: Number -> Number -> Number
(+) = (P.+)

(*) :: Number -> Number -> Number
(*) = (P.*)

div :: Number -> Number -> Number
div divisor dividendo = integralToNumber $
    P.div (numberToIntegral divisor) (numberToIntegral dividendo)

