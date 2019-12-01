import Test.Hspec

import qualified Lib as Lib

main :: IO ()
main = hspec $ do
  describe "Rocket Fuel" $ do
    it "should be 2 for a mass of 12" $ do
      Lib.rocketMassesFuel [12] `shouldBe` (2 :: Int)

    it "should be 2 for a mass of 14" $ do
      Lib.rocketMassesFuel [14] `shouldBe` (2 :: Int)

    it "should be 654 for a mass of 1969" $ do
      Lib.rocketMassesFuel [1969] `shouldBe` (654 :: Int)

    it "should be 33583 for a mass of 100756" $ do
      Lib.rocketMassesFuel [100756] `shouldBe` (33583 :: Int)

  describe "Rocket Fuel's Fuel" $ do
    it "should be 2 for a mass of 14" $ do
      Lib.rocketFuel [14] `shouldBe` (2 :: Int)

    it "should be 966 for a mass of 1969" $ do
      Lib.rocketFuel [1969] `shouldBe` (966 :: Int)

    it "should be 50346 for a mass of 100756" $ do
      Lib.rocketFuel [100756] `shouldBe` (50346 :: Int)
