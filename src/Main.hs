import Text.Printf
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)

prec :: String -> Int
prec "^" = 4
prec "*" = 3
prec "/" = 3
prec "+" = 2
prec "-" = 2
prec "log" = 5
prec _   = 0

leftAssoc :: String -> Bool
leftAssoc "^" = False
leftAssoc "log" = False
leftAssoc _ = True

isOp :: String -> Bool
isOp [t] = t `elem` "-+/*^"
isOp ('l':'o':'g':_) = True
isOp _ = False

parseLog :: String -> (String, Double)
parseLog s =
  let modVal = fromMaybe 10 (stripPrefix "log" s >>= readMaybe)
  in ("log", modVal)

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
        in (reverse beforeParen <> out,
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

evalPostfix :: [String] -> Double
evalPostfix expr = head $ foldl eval [] expr
  where
    eval :: [Double] -> String -> [Double]
    eval (x:y:ys) "+" = (y + x) : ys
    eval (x:y:ys) "-" = (y - x) : ys
    eval (x:y:ys) "*" = (y * x) : ys
    eval (x:y:ys) "/" = (y / x) : ys
    eval (x:y:ys) "^" = (y ** x) : ys
    eval (x:ys) op@('l':'o':'g':_) =
      let base = fromMaybe 10 (stripPrefix "log" op >>= readMaybe :: Maybe Double)
      in logBase base x : ys
    eval stack numStr = read numStr : stack

tokenize :: String -> [String]
tokenize "" = []
tokenize s@(c:cs)
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

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
  [(x, "")] -> Just x
  _         -> Nothing

main :: IO ()
main = do
  let expression = "log5(   2.2 + 5.5 ) * 2 + ( 7 + 8 ) ^ 2"
      tokens = tokenize expression
      result = last $ simSYA tokens
      finalResult = unwords (reverse $ fst3 result) ++ " " ++ unwords (snd3 result) ++ " " ++ trd3 result
  printf "%s\n" finalResult
  print (evalPostfix (words finalResult))

-- Utility functions
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z
