module Main where

import qualified Lib as Lib

main :: IO ()
main = do
  _ <- Lib.runProgram
  return ()
