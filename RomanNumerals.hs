import Data.List
import System.IO

-------------------
--ROMAN_TO_ARRAIC--
-------------------
roman_to_arabic :: String -> Integer
roman_to_arabic x = convertIntegers(getCharacters x)

convertIntegers :: [Integer] -> Integer
convertIntegers [] = 0
convertIntegers (x:xs) = do
  if xs /= []
    then if x < head xs
    then convertIntegers xs - x
    else
      convertIntegers xs + x
      else
      convertIntegers xs + x

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

--------------------
--ARRABIC_TO_ROMAN--
--------------------

arabic_to_roman :: Integer -> String
arabic_to_roman 0 = error "Roman Numerals must be positive numbers greater than zero"
arabic_to_roman num = calcRoman(num)

calcRoman :: Integer -> String
calcRoman 0 = []
calcRoman num = if (num > 0)
                  then if (num >= 1000)
                    then ("M" ++ (calcRoman (num-1000)))
                    else if (num >= 900)
                      then ("CM" ++ (calcRoman (num-900)))
                      else if (num >= 500)
                        then ("D" ++ (calcRoman (num-500)))
                        else if (num >= 400)
                          then ("CD" ++ (calcRoman (num-400)))
                          else if (num >= 100)
                            then ("C" ++ (calcRoman (num-100)))
                            else if (num >= 90)
                              then ("XC" ++ (calcRoman (num-90)))
                              else if (num >= 50)
                                then ("L" ++ (calcRoman (num-50)))
                                else if (num >= 40)
                                  then ("XL" ++ (calcRoman (num-40)))
                                  else if (num >= 10)
                                    then ("X" ++ (calcRoman (num-10)))
                                    else if (num >= 9)
                                      then ("IX" ++ (calcRoman (num-9)))
                                      else if (num >= 5)
                                        then ("V" ++ (calcRoman (num-5)))
                                        else if (num >= 4)
                                          then ("IV" ++ (calcRoman (num-4)))
                                          else if (num >= 1)
                                            then ("I" ++ (calcRoman (num-1)))
                                            else (error "Roman Numerals must be positive numbers greater than zero")
                                            else (error "Roman Numerals must be positive numbers greater than zero")

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
  print $ "roman_to_arabic XL - Expected: 40, Actual: " ++ show (roman_to_arabic "XL")



  print $ "** TESTING roman_to_arabic **"
  print $ "arabic_to_roman 1 - Expected: I, Actual: " ++ show (arabic_to_roman 1)
  print $ "arabic_to_roman 5 - Expected: V, Actual: " ++ show (arabic_to_roman 5)
  print $ "arabic_to_roman 10 - Expected: X, Actual: " ++ show (arabic_to_roman 10)
  print $ "arabic_to_roman 50 - Expected: L, Actual: " ++ show (arabic_to_roman 50)
  print $ "arabic_to_roman 100 - Expected: C, Actual: " ++ show (arabic_to_roman 100)
  print $ "arabic_to_roman 500 - Expected: D, Actual: " ++ show (arabic_to_roman 500)
  print $ "arabic_to_roman 1000 - Expected: M, Actual: " ++ show (arabic_to_roman 1000)

  print $ "arabic_to_roman 2 - Expected: II, Actual: " ++ show (arabic_to_roman 2)
  print $ "arabic_to_roman 9 - Expected: IX, Actual: " ++ show (arabic_to_roman 944)



  return ()
