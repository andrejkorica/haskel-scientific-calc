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
    eval (x : ys) op@('l' : 'o' : 'g' : _) =
      let base = fromMaybe 10 (stripPrefix "log" op >>= readMaybe :: Maybe Double)
       in logBase base x : ys
    eval stack numStr = read numStr : stack

-- Helper function to read a string as a number
readMaybe :: (Read a) => String -> Maybe a
readMaybe str = case reads str of
  [(x, "")] -> Just x
  _ -> Nothing
