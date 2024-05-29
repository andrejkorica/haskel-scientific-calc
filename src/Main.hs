module Main (main) where

import CalculatorGUI (handleEvent, initialState, render, update)
import ExpressionConverter (tokenize)
import Graphics.Gloss.Interface.IO.Game (Display (..), playIO, white)
import InfixPostfix (evalPostfix, simSYA)
import Text.Printf (printf)
import Utils (fst3)

main :: IO ()
main = do
  let expression = "- 5 - 2"
  case tokenize expression of
    Left errMsg -> printf "Error: %s\n" errMsg
    Right tokens -> do
      let result = last $ simSYA tokens
          finalPostfix = reverse $ fst3 result
          finalResult = unwords finalPostfix
          evaluatedResult = evalPostfix finalPostfix
      printf "Postfix Expression: %s\n" finalResult
      printf "Evaluated Result: %s\n" (show evaluatedResult)

  -- Start the graphical interface
  playIO (InWindow "Scientific calculator" (400, 550) (10, 10)) white 60 initialState render handleEvent update
