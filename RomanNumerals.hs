import Data.List
import System.IO

----------------------
--ROMAN_NUMERALS_CALCULATOR--
----------------------
add_roman_numerals :: String -> String -> String
add_roman_numerals num1 num2 = arabic_to_roman (roman_to_arabic(num1) + roman_to_arabic(num2))

subtract_roman_numerals :: String -> String -> String
subtract_roman_numerals num1 num2 =  arabic_to_roman(roman_to_arabic(num1) - roman_to_arabic(num2))

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

  print $ "** TESTING ADD_ROMAN_NUMERALS **"
  print $ "IV - I = " ++ show (subtract_roman_numerals "IV" "I")
  print $ "X + X = " ++ show (add_roman_numerals "X" "X")


  return ()
