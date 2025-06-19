import MyParser
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    it "should parse numbers" $ do
        property $ \n -> (parseMyLang $ show (getPositive n)) `shouldBe` (Right (getPositive n) :: Either String Integer)
