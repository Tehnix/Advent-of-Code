module Lib  where

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MutableVector
import Data.List.Split (splitOn)
import qualified Relude.Unsafe as Unsafe

runProgram :: IO ()
runProgram = do
  contents <- readFile "../data/prepared-input.txt"
  -- Split the contents on comma (,).
  let opcodesStrings = splitOn "," contents
      -- Convert each element to an `Int`.
      program = fmap (Unsafe.fromJust . readMaybe @Int) opcodesStrings
  -- Run the IntCode computer on the program.
  print $ "Beep boop: " <> (show $ searchForProgram program)


searchForProgram :: [Int] -> String
searchForProgram program = go program 0 0
  where
    go :: [Int] -> Int -> Int -> String
    go (res:_:_:remainingProgram) noun verb =
      let testProgram = res : noun : verb : remainingProgram
          (result:_) = intCodeComputer testProgram
      in
        case result of
          19690720 -> "noun: " <> (show noun) <> ", verb: " <> (show verb) <> ", 100 * noun + verb = " <> show (100 * noun + verb)
          _ -> if verb >= 99
            then "nope..."
            else
              if noun >= 99
              then go program 0 (verb + 1)
              else go program (noun + 1) verb

intCodeComputer :: [Int] -> [Int]
intCodeComputer program = Vector.toList $ compute (Vector.fromList program) 0
  where
    -- | Run through each instruction in the IntCode program, and handle
    -- each opcode appropriately.
    compute :: Vector Int -> Int -> Vector Int
    compute instructions pointer = do
      let opcode = instructions ! pointer
      case opcode of
        1 -> arithmetic (+) instructions pointer
        2 -> arithmetic (*) instructions pointer
        99 -> instructions -- halt the instructions
        i -> error $ "Unknown instruction: " <> show i

    -- | An arithmetic function is defined by either a `1` for addition or a `2` for
    -- multiplication.
    -- The next three instructions are its parameters, with the first two being pointers
    -- to the inputs and the third being the pointer to where the output should be stored.
    arithmetic :: (Int -> Int -> Int) -> Vector Int -> Int -> Vector Int
    arithmetic binOp instructions pointer = do
      let param1 = instructions ! (pointer + 1)
          input1 = instructions ! (param1)

          param2 = instructions ! (pointer + 2)
          input2 = instructions ! (param2)

          outputpointer = instructions ! (pointer + 3)
          -- Calculate the output based on the binary operator passed in.
          ouput = input1 `binOp` input2

          -- Write the output to the output pointer.
          newInstructions = Vector.modify (\mv -> MutableVector.write mv outputpointer ouput) instructions
      compute newInstructions (pointer + 4)
