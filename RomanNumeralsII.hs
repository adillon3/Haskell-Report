module Main where

value :: Char -> Int
value 'I' = 1
value 'V' = 5
value 'X' = 10



 getSymbols :: String -> [Int]
 getSymbols [] = []
 getSymbols (x:xs) = (value x) : getSymbols xs


main :: IO()
man = do
  let roman = "IX"
  print $getSymbols roman
