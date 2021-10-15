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



  -- calc :: [Integer] -> Integer -> Integer
  -- calc [] r = r
  -- calc (m:ms) r = do
  --   let x = m
  --   if length ms > 0
  --     then do
  --       let y = ms !! 0
  --       if x >= y
  --         then do
  --           calc ms (r + x)
  --         else do
  --           calc (tail ms) (r + y - x)
