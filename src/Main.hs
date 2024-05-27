import Data.Char (isDigit)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Interface.IO.Game
import Text.Printf ()

-- Define precedence for operators
prec :: String -> Int
prec "^" = 4
prec "*" = 3
prec "/" = 3
prec "+" = 2
prec "-" = 2
prec "log" = 5
prec _ = 0

-- Define associativity for operators
leftAssoc :: String -> Bool
leftAssoc "^" = False
leftAssoc "log" = False
leftAssoc _ = True

-- Check if a string is an operator
isOp :: String -> Bool
isOp [t] = t `elem` "-+/*^"
isOp ('l' : 'o' : 'g' : _) = True
isOp _ = False

-- Parse logarithm
parseLog :: String -> (String, Double)
parseLog s =
  let modVal = fromMaybe 10 (stripPrefix "log" s >>= readMaybe)
   in ("log", modVal)

-- Simulate Shunting Yard Algorithm
simSYA :: [String] -> [([String], [String], String)]
simSYA xs = final <> [lastStep]
  where
    final = scanl f ([], [], "") xs
    lastStep =
      ( \(x, y, _) ->
          (reverse y <> x, [], "")
      )
        $ last final
    f (out, st, _) t
      | isOp t =
          let (op, _) = parseLog t
           in ( reverse (takeWhile testOp st) <> out,
                (if op == "log" then t else op) : dropWhile testOp st,
                t
              )
      | t == "(" = (out, "(" : st, t)
      | t == ")" =
          let (beforeParen, afterParen) = span (/= "(") st
           in ( reverse beforeParen <> out,
                if null afterParen then [] else tail afterParen,
                t
              )
      | otherwise = (t : out, st, t)
      where
        testOp x =
          isOp x
            && ( leftAssoc t && prec t == prec x
                   || prec t < prec x
               )

-- Evaluate postfix expression
evalPostfix :: [String] -> Double
evalPostfix expr = head $ foldl eval [] expr
  where
    eval :: [Double] -> String -> [Double]
    eval (x : y : ys) "+" = (y + x) : ys
    eval (x : y : ys) "-" = (y - x) : ys
    eval (x : y : ys) "*" = (y * x) : ys
    eval (x : y : ys) "/" = (y / x) : ys
    eval (x : y : ys) "^" = (y ** x) : ys
    eval (x : ys) op@('l' : 'o' : 'g' : _) =
      let base = fromMaybe 10 (stripPrefix "log" op >>= readMaybe :: Maybe Double)
       in logBase base x : ys
    eval stack numStr = read numStr : stack

-- Tokenize input string
tokenize :: String -> [String]
tokenize "" = []
tokenize s@(c : cs)
  | c `elem` " \t" = tokenize cs
  | c `elem` "()+-*/^" = [c] : tokenize cs
  | c == 'l' =
      let (logOp, rest) = break (== '(') s
       in if take 3 logOp == "log"
            then logOp : tokenize (drop (length logOp) rest)
            else error "Invalid token"
  | isDigit c || c == '.' =
      let (num, rest) = span (\x -> isDigit x || x == '.') s
       in num : tokenize rest
  | otherwise = error "Invalid token"

-- Helper function to read a string as a number
readMaybe :: (Read a) => String -> Maybe a
readMaybe str = case reads str of
  [(x, "")] -> Just x
  _ -> Nothing

-- Calculator State
data CalculatorState = CalculatorState
  { displayText :: String,
    postfixExpr :: Maybe [String],
    evalResult :: Maybe Double
  }

-- Initial state
initialState :: CalculatorState
initialState = CalculatorState {displayText = "", postfixExpr = Nothing, evalResult = Nothing}

-- Main function
main :: IO ()
main = playIO (InWindow "Scientific calculator" (400, 550) (10, 10)) white 60 initialState render handleEvent update

-- Render function
render :: CalculatorState -> IO Picture
render calcState = return $ Pictures [calculator, displayTextPicture]
  where
    calculator = Pictures [drawButton (x, y) size label | (x, y, size, label) <- buttonData]
    displayTextPicture = Translate (-180) 200 $ Scale 0.25 0.25 $ Text (displayText calcState)

-- Handle events
handleEvent :: Event -> CalculatorState -> IO CalculatorState
handleEvent (EventKey (MouseButton LeftButton) Up _ (x, y)) calcState = do
  let clickedButton = findClickedButton (x, y)
  return $ case clickedButton of
    Just label -> updateDisplay label calcState
    Nothing -> calcState
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
drawButton :: (Float, Float) -> Float -> String -> Picture
drawButton (x, y) size label =
  Translate x y $
    Pictures
      [ Color buttonColor $ rectangleSolid size size,
        Color black $ rectangleWire size size,
        Translate (-textWidth / 2) (-textHeight / 2) $ Scale 0.2 0.2 $ Text label
      ]
  where
    buttonColor = makeColorI 200 200 200 255
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

-- Utility functions to get elements from a tuple
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z
