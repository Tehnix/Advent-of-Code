import Test.Hspec

import qualified Lib as Lib

main :: IO ()
main = hspec $ do
  describe "IntCode Computer" $ do
    it "should compute [1,0,0,0,99] into [2,0,0,0,99]" $ do
      Lib.intCodeComputer [1,0,0,0,99] `shouldBe` ([2,0,0,0,99] :: [Int])

    it "should compute [2,3,0,3,99] into [2,3,0,6,99]" $ do
      Lib.intCodeComputer [2,3,0,3,99] `shouldBe` ([2,3,0,6,99] :: [Int])

    it "should compute [2,4,4,5,99,0] into [2,4,4,5,99,9801]" $ do
      Lib.intCodeComputer [2,4,4,5,99,0] `shouldBe` ([2,4,4,5,99,9801] :: [Int])

    it "should compute [1,1,1,4,99,5,6,0,99] into [30,1,1,4,2,5,6,0,99]" $ do
      Lib.intCodeComputer [1,1,1,4,99,5,6,0,99] `shouldBe` ([30,1,1,4,2,5,6,0,99] :: [Int])
