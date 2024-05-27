module ExpressionConverter (tokenize) where

import Data.Char (isDigit)

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

-- TODO: Add function to convert expression to a format suitable for infix to postfix conversion
