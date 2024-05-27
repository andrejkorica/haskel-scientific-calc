module Main (main) where

import Graphics.Gloss.Interface.IO.Game (playIO, white, Display(..))
import CalculatorGUI (render, handleEvent, update, initialState)

main :: IO ()
main = playIO (InWindow "Scientific calculator" (400, 550) (10, 10)) white 60 initialState render handleEvent update