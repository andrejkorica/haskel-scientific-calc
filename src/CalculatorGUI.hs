module CalculatorGUI (main, render, handleEvent, update) where

import Graphics.Gloss.Interface.IO.Game
import InfixPostfix
import ExpressionConverter (tokenize)
import Utils (fst3, snd3, trd3)  -- Import utility functions

-- Calculator State

data CalculatorState = CalculatorState
  { displayText :: String,
    postfixExpr :: Maybe [String],
    evalResult :: Maybe Double,
    pressedButton :: Maybe String
  }

-- Initial state
initialState :: CalculatorState
initialState = CalculatorState {displayText = "", postfixExpr = Nothing, evalResult = Nothing, pressedButton = Nothing}

-- Main function
main :: IO ()
main = playIO (InWindow "Scientific calculator" (400, 550) (10, 10)) white 60 initialState render handleEvent update

-- Render function
render :: CalculatorState -> IO Picture
render calcState = return $ Pictures [calculator, displayTextPicture]
  where
    calculator = Pictures [drawButton (x, y) size label (pressedButton calcState == Just label) | (x, y, size, label) <- buttonData]
    displayTextPicture = Translate (-180) 200 $ Scale 0.25 0.25 $ Text (displayText calcState)

-- Handle events
handleEvent :: Event -> CalculatorState -> IO CalculatorState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) calcState = do
  let clickedButton = findClickedButton (x, y)
  return $ case clickedButton of
    Just label -> calcState {pressedButton = Just label}
    Nothing -> calcState
handleEvent (EventKey (MouseButton LeftButton) Up _ (x, y)) calcState = do
  let clickedButton = findClickedButton (x, y)
  return $ case clickedButton of
    Just label -> updateDisplay label calcState {pressedButton = Nothing}
    Nothing -> calcState {pressedButton = Nothing}
handleEvent _ calcState = return calcState

-- Update function
update :: Float -> CalculatorState -> IO CalculatorState
update _ = return

-- Update display based on clicked button
updateDisplay :: String -> CalculatorState -> CalculatorState
updateDisplay label calcState
  | label == "C" = calcState {displayText = "", postfixExpr = Nothing, evalResult = Nothing}
  | label == "=" =
      let tokens = tokenize (displayText calcState)
          result = last $ simSYA tokens
          postfix = reverse $ fst3 result ++ snd3 result
          evalResult = evalPostfix postfix
       in calcState {postfixExpr = Just postfix, evalResult = Just evalResult, displayText = show evalResult}
  | otherwise = calcState {displayText = displayText calcState ++ label}

-- Find clicked button
findClickedButton :: (Float, Float) -> Maybe String
findClickedButton (x, y) =
  let clickedButton = filter (\(bx, by, size, _) -> x >= bx - size / 2 && x <= bx + size / 2 && y >= by - size / 2 && y <= by + size / 2) buttonData
   in case clickedButton of
        [] -> Nothing
        (_, _, _, label) : _ -> Just label

-- Draw button
drawButton :: (Float, Float) -> Float -> String -> Bool -> Picture
drawButton (x, y) size label isPressed =
  Translate x y $
    Pictures
      [ Color (if isPressed then makeColorI 150 150 150 255 else makeColorI 200 200 200 255) $ rectangleSolid size size,
        Color black $ rectangleWire size size,
        Translate (-textWidth / 2) (-textHeight / 2) $ Scale 0.2 0.2 $ Text label
      ]
  where
    textWidth = case label of
      "log" -> 60
      "ln" -> 40
      "DEL" -> 40
      "log2" -> 60
      "2^x" -> 60
      "x^y" -> 60
      "e^x" -> 60
      _ -> fromIntegral $ length label * 35
    textHeight = 35

-- Button data
buttonData :: [(Float, Float, Float, String)]
buttonData =
  [ (-150, 150, 65, "e"),
    (-75, 150, 65, "("),
    (0, 150, 65, ")"),
    (75, 150, 65, "C"),
    (150, 150, 65, "DEL"),
    (-150, 75, 65, "x^y"),
    (-75, 75, 65, "7"),
    (0, 75, 65, "8"),
    (75, 75, 65, "9"),
    (150, 75, 65, "/"),
    (-150, 0, 65, "e^x"),
    (-75, 0, 65, "4"),
    (0, 0, 65, "5"),
    (75, 0, 65, "6"),
    (150, 0, 65, "*"),
    (-150, -75, 65, "log"),
    (-75, -75, 65, "1"),
    (0, -75, 65, "2"),
    (75, -75, 65, "3"),
    (150, -75, 65, "-"),
    (-150, -150, 65, "ln"),
    (-75, -150, 65, "0"),
    (0, -150, 65, "."),
    (75, -150, 65, "="),
    (150, -150, 65, "+"),
    (-150, -225, 65, "2^x"),
    (-75, -225, 65, "log2")
  ]