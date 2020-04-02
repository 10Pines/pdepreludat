import PdePreludat
import Library
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2

