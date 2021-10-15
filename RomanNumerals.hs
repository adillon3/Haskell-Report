import Data.List
import System.IO

-- --Data Types
-- -- I = 1, V = 5, X = 10, L = 50, C = 100, D = 500, M = 1000
-- data ROMAN = I | V | X | L | C | D | M | ROMAN ROMAN
--   deriving (Eq,Show) -- for equality and printing

roman_to_arabic :: String -> Integer
roman_to_arabic x = convertIntegers(getCharacters x)

-- convert :: [Integer] -> Integer
-- convert list = calc list 0

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






convertIntegers :: [Integer] -> Integer
convertIntegers [] = 0
convertIntegers (x:xs) = x + convertIntegers xs
--if the previous value is less than the current value, then we need to subtract it from the next value, ie: if we have IV, we subtract 1 from 5 and return that instead


getCharacters :: String -> [Integer]
getCharacters [] = []
getCharacters (x:xs) = (getValueOfCharacter x) : getCharacters xs

getValueOfCharacter :: Char -> Integer
getValueOfCharacter 'I' = 1
getValueOfCharacter 'V' = 5
getValueOfCharacter 'X' = 10
getValueOfCharacter 'L' = 50
getValueOfCharacter 'C' = 100
getValueOfCharacter 'D' = 500
getValueOfCharacter 'M' = 1000

-- --Converting roman numerals to arabic numerals/integers
-- roman_to_arabic :: String -> Integer
-- roman_to_arabic "I" = 1
-- roman_to_arabic "V" = 5
-- roman_to_arabic "X" = 10
-- roman_to_arabic "L" = 50
-- roman_to_arabic "C" = 100
-- roman_to_arabic "D" = 500
-- roman_to_arabic "M" = 1000

-- roman_to_arabic (val I) = (roman_to_arabic val) + 1
-- roman_to_arabic V val = (roman_to_arabic val) + 5
-- roman_to_arabic X val = (roman_to_arabic val) + 10
-- roman_to_arabic L val = (roman_to_arabic val) + 50
-- roman_to_arabic C val = (roman_to_arabic val) + 100
-- roman_to_arabic D val = (roman_to_arabic val) + 500
-- roman_to_arabic M val = (roman_to_arabic val) + 1000


--Converting arabic numerals/integers to roman numerals
-- arabic_to_roman :: Integer -> ROMAN
-- arabic_to_roman 1 = I
-- arabic_to_roman 5 = V
-- arabic_to_roman 10 = X
-- arabic_to_roman 50 = L
-- arabic_to_roman 100 = C
-- arabic_to_roman 500 = D
-- arabic_to_roman 1000 = M

-- arabic_to_roman num = if (num < 0)
--                        then (error "Roman Numerals must be positive numbers greater than zero")
--                        else if (num > 1000)
--                          then (M (arabic_to_roman (num-1000)))
--                          else if (num > 500)
--                            then (D (arabic_to_roman (num-500)))
--                            else if (num > 100)
--                              then (C (arabic_to_roman (num-100)))
--                              else if (num > 100)
--                                then (L (arabic_to_roman (num-50)))
--                                else if (num > 10)
--                                  then (X (arabic_to_roman (num-10)))
--                                  else if (num > 5)
--                                    then (V (arabic_to_roman (num-5)))
--                                    else if (num > 1)
--                                      then (I (arabic_to_roman (num-1)))
--                                      else (error "Roman Numeral could not be found")



----------
-- Testing
----------

main = do
  -- print $ "** TESTING arabic_to_roman **"
  -- print $ "arabic_to_roman 1 - Expected: I, Actual: " ++ show (arabic_to_roman 1)
  -- print $ "arabic_to_roman 5 - Expected: V, Actual: " ++ show (arabic_to_roman 5)
  -- print $ "arabic_to_roman 10 - Expected: X, Actual: " ++ show (arabic_to_roman 10)
  -- print $ "arabic_to_roman 50 - Expected: L, Actual: " ++ show (arabic_to_roman 50)
  -- print $ "arabic_to_roman 100 - Expected: C, Actual: " ++ show (arabic_to_roman 100)
  -- print $ "arabic_to_roman 500 - Expected: D, Actual: " ++ show (arabic_to_roman 500)
  -- print $ "arabic_to_roman 1000 - Expected: M, Actual: " ++ show (arabic_to_roman 1000)

  print $ "** TESTING roman_to_arabic **"
  print $ "roman_to_arabic I - Expected: 1, Actual: " ++ show (roman_to_arabic "I")
  print $ "roman_to_arabic V - Expected: 5, Actual: " ++ show (roman_to_arabic "V")
  print $ "roman_to_arabic X - Expected: 10, Actual: " ++ show (roman_to_arabic "X")
  print $ "roman_to_arabic L - Expected: 50, Actual: " ++ show (roman_to_arabic "L")
  print $ "roman_to_arabic C - Expected: 100, Actual: " ++ show (roman_to_arabic "C")
  print $ "roman_to_arabic D - Expected: 500, Actual: " ++ show (roman_to_arabic "D")
  print $ "roman_to_arabic M - Expected: 1000, Actual: " ++ show (roman_to_arabic "M")


  print $ "roman_to_arabic II - Expected: 2, Actual: " ++ show (roman_to_arabic "II")
  print $ "roman_to_arabic IV - Expected: 4, Actual: " ++ show (roman_to_arabic "IV")


  -- let roman = "IX"
  -- let list = getSymbols roman
  -- print $ convert list


  return ()
