module Main (main) where

import Graphics.Gloss.Interface.IO.Game (playIO, white, Display(..))
import CalculatorGUI (render, handleEvent, update, initialState)
import InfixPostfix (simSYA, evalPostfix)
import ExpressionConverter (tokenize)
import Utils (fst3, snd3)
import Text.Printf (printf)

-- Define trd3 utility function
trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

main :: IO ()
main = do
  let expression = "log5(5 + 2)"
      tokens = tokenize expression
      result = last $ simSYA tokens
      finalPostfix = reverse $ fst3 result
      finalResult = unwords finalPostfix
      evaluatedResult = evalPostfix finalPostfix
  printf "Postfix Expression: %s\n" finalResult
  printf "Evaluated Result: %.2f\n" evaluatedResult

  -- Start the graphical interface
  playIO (InWindow "Scientific calculator" (400, 550) (10, 10)) white 60 initialState render handleEvent update
