import Data.List
import System.IO
import Data.Char

-----------------------------
--ROMAN_NUMERALS_CALCULATOR--
-----------------------------
addRomanNumerals :: String -> String -> String
addRomanNumerals num1 num2 = arabicToRoman (romanToArabic(num1) + romanToArabic(num2))

subtractRomanNumerals :: String -> String -> String
subtractRomanNumerals num1 num2 =  arabicToRoman(romanToArabic(num1) - romanToArabic(num2))

-------------------
--ROMAN TO ARABIC--
-------------------
romanToArabic :: String -> Integer
romanToArabic romanInput = convertIntegers(getCharacters romanInput)

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
getCharacters (x:xs) = (getValueOfCharacter (toUpper x)) : getCharacters xs

getValueOfCharacter :: Char -> Integer
getValueOfCharacter 'I' = 1
getValueOfCharacter 'V' = 5
getValueOfCharacter 'X' = 10
getValueOfCharacter 'L' = 50
getValueOfCharacter 'C' = 100
getValueOfCharacter 'D' = 500
getValueOfCharacter 'M' = 1000
getValueOfCharacter x = error "Invalid character: Roman Numerals can only use I, V, L, X, C, D, & M"

--------------------
--ARRABIC TO ROMAN--
--------------------

arabicToRoman :: Integer -> String
arabicToRoman 0 = error "Roman Numerals must be positive numbers greater than zero"
arabicToRoman num = calcRoman(num)

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

  print $ "** TESTING addRomanNumerals **"
  print $ "IV - I = " ++ show (subtractRomanNumerals "IV" "I")
  print $ "X + X = " ++ show (addRomanNumerals "X" "x")


  return ()
