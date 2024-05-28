module ExpressionConverter (tokenize) where

import Data.Char (isDigit)
import Debug.Trace (trace)

-- Tokenize input string
tokenize :: String -> [String]
tokenize s = tokenizeHelper s True

-- Helper function to handle the first token differently
tokenizeHelper :: String -> Bool -> [String]
tokenizeHelper "" _ = []
tokenizeHelper s@(c : cs) isFirst
  | c `elem` " \t" = trace ("Tokenizing space/tab: " ++ [c]) $ tokenizeHelper cs isFirst
  | c `elem` "()+-*/^" =
      if isFirst && c == '-'
      then case tokenizeHelper cs False of
              (num:rest) -> trace ("Tokenizing negative number: " ++ ('-':num)) $ ('-':num) : rest
              [] -> error "Invalid expression: lone minus sign"
      else trace ("Tokenizing operator: " ++ [c]) $ [c] : tokenizeHelper cs False
  | c == '(' =
      let openParen = c : replicate 3 ' '
          rest = dropWhile (`elem` " \t") cs
       in trace ("Tokenizing open parenthesis: " ++ openParen) $ openParen : tokenizeHelper rest False
  | c == ')' = trace ("Tokenizing close parenthesis: " ++ [c]) $ [c] : tokenizeHelper cs False
  | c == 'l' =
      let (funOperator, rest) = break (== '(') s
       in if take 3 funOperator == "log" || funOperator == "ln"
            then
              let rest' = dropWhile (`elem` " \t") rest
               in trace ("Tokenizing function operator: " ++ funOperator) $ funOperator : tokenizeHelper rest' False
            else error "Invalid token"
  | isDigit c || c == '.' =
      let (num, rest) = span (\x -> isDigit x || x == '.') s
       in trace ("Tokenizing number: " ++ num) $ num : tokenizeHelper rest False
  | c == 'e' = trace "Tokenizing constant: e" $ "2.71828182846" : tokenizeHelper cs False
  | otherwise = error "Invalid token"
