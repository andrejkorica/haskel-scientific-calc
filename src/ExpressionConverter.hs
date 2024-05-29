module ExpressionConverter (tokenize) where

import Data.Char (isDigit)
import Debug.Trace (trace)

-- Tokenize input string
tokenize :: String -> Either String [String]
tokenize s = tokenizeHelper s True False

-- Helper function to handle the first token and operator-followed minus differently
tokenizeHelper :: String -> Bool -> Bool -> Either String [String]
tokenizeHelper "" _ _ = Right []
tokenizeHelper s@(c : cs) isFirst wasOperator
  | c `elem` " \t" = trace ("Tokenizing space/tab: " ++ [c]) $ tokenizeHelper cs isFirst wasOperator
  | c `elem` "()+-*/^" =
      if isFirst && c == '-'
        then case tokenizeHelper cs False False of
          Right (num : rest) -> trace ("Tokenizing negative number: " ++ ('-' : num)) $ Right (('-' : num) : rest)
          Right [] -> Left "Invalid expression: lone minus sign"
          Left err -> Left err
        else
          if wasOperator && c == '-'
            then case tokenizeHelper cs False False of
              Right (num : rest) -> trace ("Tokenizing negative number after operator: " ++ ('-' : num)) $ Right (('-' : num) : rest)
              Right [] -> Left "Invalid expression: lone minus sign after operator"
              Left err -> Left err
            else trace ("Tokenizing operator: " ++ [c]) $ fmap ([c] :) (tokenizeHelper cs False True)
  | c == '(' =
      let openParen = c : replicate 3 ' '
          rest = dropWhile (`elem` " \t") cs
       in trace ("Tokenizing open parenthesis: " ++ openParen) $ fmap (openParen :) (tokenizeHelper rest False False)
  | c == ')' = trace ("Tokenizing close parenthesis: " ++ [c]) $ fmap ([c] :) (tokenizeHelper cs False False)
  | c == 'l' =
      let (funOperator, rest) = break (== '(') s
       in if take 3 funOperator == "log" || funOperator == "ln"
            then
              let rest' = dropWhile (`elem` " \t") rest
               in trace ("Tokenizing function operator: " ++ funOperator) $ fmap (funOperator :) (tokenizeHelper rest' False False)
            else Left "Invalid token"
  | isDigit c || c == '.' =
      let (num, rest) = span (\x -> isDigit x || x == '.') s
       in trace ("Tokenizing number: " ++ num) $ fmap (num :) (tokenizeHelper rest False False)
  | c == 'e' = trace "Tokenizing constant: e" $ fmap ("2.71828182846" :) (tokenizeHelper cs False False)
  | otherwise = Left "Invalid token"
