module Main where
import PdePreludat
import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $
  describe "Number" $ do
    it "dividir por 0 lanza error" $ do
      shouldThrowError $ 1 / 0
    describe "redondea a 9 decimales para compensar errores de punto flotante" $ do
      it "los numeros con muchos decimales se muestran redondeados" $ do
        show 0.9999999999 `shouldBe` "1.0"
      it "los decimales literales se redondean" $ do
        0.9999999999 `shouldBeTheSameNumberAs` 1
      it "los resultados de sumas se redondean" $ do
        (0.1 + 0.7) `shouldBeTheSameNumberAs` 0.8
      it "los resultados de restas se redondean" $ do
        (0.8 - 0.1) `shouldBeTheSameNumberAs` 0.7
      it "los resultados de multiplicaciones se redondean" $ do
        (0.1 * 3) `shouldBeTheSameNumberAs` 0.3
      it "los resultados de divisiones se redondean" $ do
        (1 / 3) `shouldBeTheSameNumberAs` 0.333333333
      it "no se pierde informacion al redondear en sucesivas operaciones" $ do
        (1 / 3 * 3) `shouldBeTheSameNumberAs` 1
      it "se redondea al usarse como parámetro de funciones que necesitan enteros" $ do
        take 0.9999999999 [1,2,3,4] `shouldBe` [1]

shouldBeTheSameNumberAs :: Number -> Number -> Expectation
shouldBeTheSameNumberAs aNumber anotherNumber =
  (aNumber `shouldBe` anotherNumber) <>
  (aNumber < anotherNumber `shouldBe` False) <>
  (aNumber > anotherNumber `shouldBe` False)

shouldThrowError expresion = evaluate expresion `shouldThrow` anyException