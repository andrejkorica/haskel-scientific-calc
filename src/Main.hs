-- Define a data type for representing operators
data Operator = Add | Subtract | Multiply | Divide deriving (Show, Eq)

-- Define a function to convert a string representation of an operator to its corresponding Operator type
operatorFromString :: String -> Operator
operatorFromString "+" = Add
operatorFromString "-" = Subtract
operatorFromString "*" = Multiply
operatorFromString "/" = Divide
operatorFromString _ = error "Invalid operator"

-- Define a function to determine the precedence of an operator
precedence :: Operator -> Int
precedence Add = 1
precedence Subtract = 1
precedence Multiply = 2
precedence Divide = 2

-- Define a function to check if a string represents a number
isNumber :: String -> Bool
isNumber s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

-- Define a function to convert an infix expression to a postfix expression using the Shunting Yard algorithm
infixToPostfix :: [String] -> [String]
infixToPostfix tokens = go tokens [] []
  where
    go :: [String] -> [String] -> [Operator] -> [String]
    go [] output stack = output ++ map show (reverse stack)
    go (token:rest) output stack
      | isNumber token = go rest (output ++ [token]) stack
      | token == "(" = go rest output (operatorFromString token : stack)
      | token == ")" =
          let (output', stack') = span (/= Add) stack
          in go rest (output ++ map show output') (tail stack')
      | token `elem` ["+", "-", "*", "/"] =
          let (greaterEq, stack') = span (\op -> precedence op >= precedence (operatorFromString token)) stack
          in go rest (output ++ map show greaterEq) (operatorFromString token : stack')
      | otherwise = error $ "Invalid token: " ++ token

-- Example usage:
main :: IO ()
main = do
  let expression = "2 * 2 + 7 + 8"
  let tokens = words expression
  let postfix = infixToPostfix tokens
  putStrLn $ "Expression: " ++ expression
  putStrLn $ "Postfix: " ++ unwords postfix
