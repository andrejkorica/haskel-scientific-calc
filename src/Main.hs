import Text.Printf

prec :: String -> Int
prec "^" = 4
prec "*" = 3
prec "/" = 3
prec "+" = 2
prec "-" = 2
prec _   = 0

leftAssoc :: String -> Bool
leftAssoc "^" = False
leftAssoc _ = True

isOp :: String -> Bool
isOp [t] = t `elem` "-+/*^"
isOp _ = False

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
        ( reverse (takeWhile testOp st) <> out,
          t : dropWhile testOp st,
          t
        )
      | t == "(" = (out, "(" : st, t)
      | t == ")" =
        ( reverse (takeWhile (/= "(") st) <> out,
          tail $ dropWhile (/= "(") st,
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
    eval stack numStr = read numStr : stack

main :: IO ()
main = do
  let expression = "2.2 * 2 + ( 7 + 8 ) ^ 2"
      result = last $ simSYA $ words expression
      finalResult = unwords (reverse $ fst3 result) ++ " " ++ unwords (snd3 result) ++ " " ++ trd3 result
  printf "%s\n" finalResult
  print (evalPostfix (words finalResult))


fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z
