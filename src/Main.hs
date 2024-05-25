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
prec "ln" = 5
prec "log2" = 5
prec "2^x" = 5
prec "x^e" = 5
prec "e^x" = 5
prec _ = 0

-- Define associativity for operators
leftAssoc :: String -> Bool
leftAssoc "^" = False
leftAssoc "log" = False
leftAssoc "ln" = False
leftAssoc "log2" = False
leftAssoc "2^x" = False
leftAssoc "x^e" = False
leftAssoc "e^x" = False
leftAssoc _ = True

-- Check if a string is an operator
isOp :: String -> Bool
isOp [t] = t `elem` "-+/*^"
isOp ('l' : 'o' : 'g' : _) = True
isOp "ln" = True
isOp "log2" = True
isOp "2^x" = True
isOp "x^e" = True
isOp "e^x" = True
isOp _ = False

-- Parse logarithm
parseLog :: String -> (String, Double)
parseLog s
  | take 2 s == "ln" = ("ln", exp 1)
  | s == "log2" = ("log2", 2)
  | otherwise =
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
                (if op `elem` ["log", "ln", "log2", "2^x", "x^e", "e^x"] then t else op) : dropWhile testOp st,
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
    eval (x : ys) "ln" = log x : ys
    eval (x : ys) "log2" = logBase 2 x : ys
    eval (x : ys) "2^x" = (2 ** x) : ys
    eval (x : ys) "x^e" = (x ** exp 1) : ys
    eval (x : ys) "e^x" = (exp x) : ys
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
  | c == 'l' || c == '2' || c == 'x' || c == 'e' =
      let (logOp, rest) = break (== '(') s
       in if take 3 logOp == "log" || logOp == "ln" || logOp == "log2" || logOp == "2^x" || logOp == "x^e" || logOp == "e^x"
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
main = playIO (InWindow "Scientific calculator" (400, 500) (10, 10)) white 60 initialState render handleEvent update

-- Render function
render :: CalculatorState -> IO Picture
render calcState = return $ Pictures [calculator, displayTextPicture]
  where
    calculator = Pictures [drawButton (x, y) size label | (x, y, size, label) <- buttonData]
    displayTextPicture = Translate (-160) 250 $ Scale 0.2 0.2 $ Text (displayText calcState)

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
      "log2" -> 70
      "2^x" -> 60
      "x^e" -> 60
      "e^x" -> 60
      _ -> fromIntegral $ length label * 35
    textHeight = 35

-- Button data
buttonData :: [(Float, Float, Float, String)]
buttonData =
  [ (-150, 100, 80, "1"),
    (-50, 100, 80, "2"),
    (50, 100, 80, "3"),
    (150, 100, 80, "+"),
    (-150, 0, 80, "4"),
    (-50, 0, 80, "5"),
    (50, 0, 80, "6"),
    (150, 0, 80, "-"),
    (-150, -100, 80, "7"),
    (-50, -100, 80, "8"),
    (50, -100, 80, "9"),
    (150, -100, 80, "*"),
    (-150, -200, 80, "C"),
    (-50, -200, 80, "0"),
    (50, -200, 80, "."),
    (150, -200, 80, "/"),
    (-150, -300, 80, "("),
    (-50, -300, 80, ")"),
    (50, -300, 80, "log"),
    (150, -300, 80, "ln"),
    (-150, -400, 80, "log2"),
    (-50, -400, 80, "2^x"),
    (50, -400, 80, "x^e"),
    (150, -400, 80, "e^x"),
    (150, 200, 80, "=") -- Adjusted "=" button position
  ]

-- Utility functions to get elements from a tuple
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z
