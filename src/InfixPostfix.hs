module InfixPostfix (prec, leftAssoc, isOp, parseLog, simSYA, evalPostfix, readMaybe) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

-- Define precedence for operators
prec :: String -> Int
prec "^" = 4
prec "*" = 3
prec "/" = 3
prec "+" = 2
prec "-" = 2
prec "log" = 5
prec "ln" = 5
prec _ = 0

-- Define associativity for operators
leftAssoc :: String -> Bool
leftAssoc "^" = False
leftAssoc "log" = False
leftAssoc "ln" = False
leftAssoc _ = True

-- Check if a string is an operator
isOp :: String -> Bool
isOp [t] = t `elem` "-+/*^"
isOp ('l' : 'o' : 'g' : _) = True
isOp ('l' : 'n' : _) = True
isOp _ = False

-- Parse logarithm
parseLog :: String -> (String, Double)
parseLog calcFuncStr
  | calcFuncStr == "ln" = ("ln", exp 1)
  | otherwise =
      let modVal = fromMaybe 10 (stripPrefix "log" calcFuncStr >>= readMaybe)
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
                (if op == "log" || op == "ln" then t else op) : dropWhile testOp st,
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
evalPostfix :: [String] -> Either String Double
evalPostfix expr =
  case foldl eval (Right []) expr of
    Right [result] -> Right result
    Right _ -> Left "Error: Invalid expression"
    Left err -> Left err
  where
    eval :: Either String [Double] -> String -> Either String [Double]
    eval (Right (x : y : ys)) "+" = Right ((y + x) : ys)
    eval (Right (x : y : ys)) "-" = Right ((y - x) : ys)
    eval (Right (x : y : ys)) "*" = Right ((y * x) : ys)
    eval (Right (x : y : ys)) "/" = if x /= 0 then Right ((y / x) : ys) else Left "Error: Division by zero"
    eval (Right (x : y : ys)) "^" = Right ((y ** x) : ys)
    eval (Right (x : ys)) "ln" = if x > 0 then Right (log x : ys) else Left "Error: Logarithm of non-positive number"
    eval (Right (x : ys)) op@('l' : 'o' : 'g' : _) =
      let base = fromMaybe 10 (stripPrefix "log" op >>= readMaybe :: Maybe Double)
       in if x > 0 then Right (logBase base x : ys) else Left "Error: Logarithm of non-positive number"
    eval (Right stack) numStr = case readMaybe numStr of
      Just num -> Right (num : stack)
      Nothing -> Left "Error: Invalid number"
    eval (Left err) _ = Left err

-- Helper function to read a string as a number
readMaybe :: (Read a) => String -> Maybe a
readMaybe str = case reads str of
  [(x, "")] -> Just x
  _ -> Nothing
