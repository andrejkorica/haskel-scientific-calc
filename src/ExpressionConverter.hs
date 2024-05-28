module ExpressionConverter (tokenize) where

import Data.Char (isDigit)
import Debug.Trace (trace)

-- Tokenize input string
tokenize :: String -> [String]
tokenize "" = []
tokenize s@(c : cs)
  | c `elem` " \t" = trace ("Tokenizing space/tab: " ++ [c]) $ tokenize cs
  | c `elem` "()+-*/^" = trace ("Tokenizing operator: " ++ [c]) $ [c] : tokenize cs
  | c == '(' =
      let openParen = c : replicate 3 ' '
          rest = dropWhile (`elem` " \t") cs
       in trace ("Tokenizing open parenthesis: " ++ openParen) $ openParen : tokenize rest
  | c == ')' = trace ("Tokenizing close parenthesis: " ++ [c]) $ [c] : tokenize cs
  | c == 'l' =
      let (funOperator, rest) = break (== '(') s
       in if take 3 funOperator == "log" || funOperator == "ln"
            then let rest' = dropWhile (`elem` " \t") rest
                 in trace ("Tokenizing function operator: " ++ funOperator) $ funOperator : tokenize rest'
            else error "Invalid token"
  | isDigit c || c == '.' =
      let (num, rest) = span (\x -> isDigit x || x == '.') s
       in trace ("Tokenizing number: " ++ num) $ num : tokenize rest
  | otherwise = error "Invalid token"
