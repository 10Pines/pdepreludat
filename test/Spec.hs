module Main where
import PdePreludat
import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Pdepreludat" $ do
    describe "expresion if then else" $ do
      it "si la condicion del if es verdadera, devuelve el valor del then" $ do
        (if 2 == 2 then "Then" else "Else") `shouldBe` "Then"
      it "si la condicion del if es falsa, devuelve el valor del else" $ do
        (if 2 == 3 then 1 else 0) `shouldBe` 0

    describe "Number" $ do
      it "dividir por 0 lanza error" $ do
        shouldThrowError $ 1 / 0
      describe "Show" $ do
        it "mostrar un numero entero no muestra los decimales" $ do
          show 1 `shouldBe` "1"
        it "mostrar un numero decimal incluye la parte decimal" $ do
          show 1.5 `shouldBe` "1.5"
      describe "redondea a 9 decimales para compensar errores de punto flotante" $ do
        it "los numeros con muchos decimales se muestran redondeados" $ do
          show 0.9999999999 `shouldBe` "1"
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