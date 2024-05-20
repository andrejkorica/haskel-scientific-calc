-- Define a data type for representing operators
data Operator = Add | Subtract | Multiply | Divide | OpenParen | CloseParen deriving (Show, Eq)

-- Define a function to convert a string representation of an operator to its corresponding Operator type
operatorFromString :: String -> Operator
operatorFromString "+" = Add
operatorFromString "-" = Subtract
operatorFromString "*" = Multiply
operatorFromString "/" = Divide
operatorFromString "(" = OpenParen
operatorFromString ")" = CloseParen
operatorFromString _ = error "Invalid operator"

-- Define a function to determine the precedence of an operator
precedence :: Operator -> Int
precedence Add = 1
precedence Subtract = 1
precedence Multiply = 2
precedence Divide = 2
precedence OpenParen = 0
precedence CloseParen = 0

-- Define a function to check if a string represents a number
isNumber :: String -> Bool
isNumber s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

infixToPostfix :: [String] -> [String]
infixToPostfix tokens = go tokens [] []
  where
    go :: [String] -> [String] -> [String] -> [String]
    go [] output stack = output ++ reverse stack
    go (token:rest) output stack
      | token == "(" = go rest output (token : stack)
      | token == ")" =
          let (output', stack') = break (== "(") stack
          in if null stack'
              then error "Mismatched parentheses"
              else go rest (output ++ reverse output') (tail stack')
      | isNumber token = go rest (output ++ [token]) stack
      | token `elem` ["+", "-", "*", "/"] =
          let currentOp = operatorFromString token
              (greaterEq, stack') = span (\op -> precedence (operatorFromString op) >= precedence currentOp) stack
          in go rest (output ++ reverse greaterEq) (token : stack')
      | otherwise = error $ "Invalid token: " ++ token


-- Example usage:
main :: IO ()
main = do
  let expression = "2 * 2 + ( 7 + 8 )"
  let tokens = words expression
  let postfix = infixToPostfix tokens
  putStrLn $ "Expression: " ++ expression
  putStrLn $ "Postfix: " ++ unwords postfix
