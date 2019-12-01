module Lib  where

import qualified Relude.Unsafe as Unsafe

-- | Calculate the fuel cost, given a list of masses.
--
-- The formula goes:
-- · Take the mass
-- · Divide it by 3
-- · Round down
-- · Subtract 2
calculateFuel :: Int -> Int
calculateFuel mass =
  let divided = (fromIntegral mass) / 3.0
      floored = floor (divided)
      fuelCost = floored - 2
  in fuelCost

-- | Calculate the fuel cost of the fuel. Do this recursively until
-- we hit a negative fuel.
calculateFuelsFuel :: Int -> Int
calculateFuelsFuel fuelMass =
  let additionalFuel = calculateFuel fuelMass
  in
    if additionalFuel > 0
      then additionalFuel + calculateFuelsFuel additionalFuel
      else 0

rocketMassesFuel :: [Int] -> Int
rocketMassesFuel masses = sum $ map calculateFuel masses

rocketFuel :: [Int] -> Int
rocketFuel masses = sum $ map totalFuel masses
  where
    totalFuel :: Int -> Int
    totalFuel mass =
      let massFuel = sum $ map calculateFuel [mass]
          additionalFuel = calculateFuelsFuel massFuel
      in massFuel + additionalFuel

runProgram :: IO ()
runProgram = do
  fileLines <- readFileIntoLines "./data/input.txt"
  let masses = map (Unsafe.fromJust . readMaybe @Int . toString) fileLines
  print $ "Part One: " <> (show $ rocketMassesFuel masses)
  print $ "Part Two: " <> (show $ rocketFuel masses)


-- | Read a file and split it on newlines.
readFileIntoLines :: FilePath -> IO [Text]
readFileIntoLines name = do
  contents <- readFile name
  return $ lines $ toText contents
