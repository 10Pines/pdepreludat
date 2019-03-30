import PdePreludat
import Library
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Example test" $ do
    it "The answer to all questions is..." $ do
      someFunc1 `shouldBe` 42
