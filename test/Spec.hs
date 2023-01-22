module Main where
import PdePreludat
import Test.Hspec
import Control.Exception (evaluate)
import qualified Prelude as P
import Test.Hspec.QuickCheck
import Test.QuickCheck

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
      describe "no hay errores de punto flotante al usar numeros decimales" $ do
        it "números enteros más allá de la precisión de Double se muestran correctamente" $ do
          show 12345678901234567890 `shouldBe` "12345678901234567890"
        it "números decimales más allá de la precisión de Double se muestran correctamente" $ do
          show 12345678901234567890.7 `shouldBe` "12345678901234567890.7"
        it "numeros decimales negativos se muestran correctamente" $ do
          show (-0.5) `shouldBe` "-0.5"
        it "numeros decimales muy grandes se muestran correctamente" $ do
          show (-12345678901234567890.3) `shouldBe` "-12345678901234567890.3"
        it "los numeros decimales positivos NO se muestran en notacion cientifica" $ do
          show 0.000001 `shouldBe` "0.000001"
        it "los numeros decimales negativos NO se muestran en notacion cientifica" $ do
          show (-0.000001) `shouldBe` "-0.000001"
        it "los resultados de sumas dan el resultado esperado" $ do
          (0.1 + 0.7) `shouldBeTheSameNumberAs` 0.8
        it "los resultados de restas dan el resultado esperado" $ do
          (0.8 - 0.1) `shouldBeTheSameNumberAs` 0.7
        it "los resultados de multiplicaciones dan el resultado esperado" $ do
          (0.1 * 3) `shouldBeTheSameNumberAs` 0.3
        it "no se pierde informacion al hacer sucesivas operaciones con decimales" $ do
          (1 / 3 * 3) `shouldBeTheSameNumberAs` 1
      describe "Floating" $ do
        it "pi se muestra correctamente" $ do
          take 10 (show pi) `shouldBe` "3.14159265"
        it "las funciones trigonometricas funcionan correctamente" $ do
          cos pi `shouldBe` -1
          sin (pi / 2) `shouldBe` 1

    describe "enumFromThenTo" $ do
      describe "cuando todos los numeros son enteros" $ do
        it "devuelve una lista del primero al ultimo inclusive usando como salto entre cada numero la diferencia entre los dos primeros" $ do
          [1, 2 .. 5] `shouldBe` [1, 2, 3, 4, 5]
          [0, 2 .. 4] `shouldBe` [0, 2, 4]
          [0, 2 .. 5] `shouldBe` [0, 2, 4]

          [-1, -2 .. -5] `shouldBe` [-1, -2, -3, -4, -5]
          [0, -2 .. -4] `shouldBe` [0, -2, -4]
          [0, -2 .. -5] `shouldBe` [0, -2, -4]

          [4, 3 .. 1] `shouldBe` [4 , 3, 2, 1]
          [4, 2 .. 1] `shouldBe` [4 , 2]
          [1 .. 10] `shouldBe` [1,2,3,4,5,6,7,8,9,10]
      describe "cuando hay numeros decimales" $ do
        -- El comportamiento de este caso está sacado de acá:
        -- https://www.haskell.org/onlinereport/basic.html
        -- For Float and Double, the semantics of the enumFrom family is given by the rules for Int above,
        -- except that the list terminates when the elements become greater than e3+i/2 for positive increment i,
        -- or when they become less than e3+i/2 for negative i.
        it "devuelve una lista del primero al ultimo usando como salto entre cada numero la diferencia entre los dos primeros, la lista termina cuando aparece un elemento cuyo valor es más grande que el tercer parámetro + la mitad del salto" $ do
          [1, 2 .. 5.5] `shouldBe` [1, 2, 3, 4, 5, 6]
          [1, 2.5 .. 5.5] `shouldBe` [1, 2.5, 4, 5.5]
          [0, 2 .. 5.5] `shouldBe` [0, 2, 4, 6]
          [0, 2.5 .. 5.5] `shouldBe` [0, 2.5, 5]

          [5, 4 .. 1.5] `shouldBe` [5, 4, 3, 2, 1]
          [5.5, 4 .. 1.5] `shouldBe` [5.5, 4, 2.5, 1]
          [5.5, 4 .. 1] `shouldBe` [5.5, 4, 2.5, 1]

        describe "sumOf => sum de orden superior" $ do
          it "devuelve 0 para una lista vacía" $ do
            sumOf length [] `shouldBe` 0
          it "aplica la función para una lista con elementos" $ do
            sumOf length ["abracadabra", "pata", "de", "cabra"] `shouldBe` 22

shouldBeTheSameNumberAs :: HasCallStack => Number -> Number -> Expectation
shouldBeTheSameNumberAs aNumber anotherNumber =
  (aNumber `shouldBe` anotherNumber) <>
  (aNumber < anotherNumber `shouldBe` False) <>
  (aNumber > anotherNumber `shouldBe` False)

shouldThrowError :: a -> Expectation
shouldThrowError expresion = evaluate expresion `shouldThrow` anyException