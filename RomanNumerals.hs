import Data.List
import System.IO

--Data Types
-- I = 1, V = 5, X = 10, L = 50, C = 100, D = 500, M = 1000
data ROMAN = I | V | X | L | C | D | M
  deriving (Eq,Show) -- for equality and printing


--Converting roman numerals to arabic numerals/integers
roman_to_arabic :: ROMAN -> Integer
roman_to_arabic I = 1
roman_to_arabic V = 5
roman_to_arabic X = 10
roman_to_arabic L = 50
roman_to_arabic C = 100
roman_to_arabic D = 500
roman_to_arabic M = 1000

roman_to_arabic I val = (roman_to_arabic val) + 1
roman_to_arabic V val = (roman_to_arabic val) + 5
roman_to_arabic X val = (roman_to_arabic val) + 10
roman_to_arabic L val = (roman_to_arabic val) + 50
roman_to_arabic C val = (roman_to_arabic val) + 100
roman_to_arabic D val = (roman_to_arabic val) + 500
roman_to_arabic M val = (roman_to_arabic val) + 1000


--Converting arabic numerals/integers to roman numerals
arabic_to_roman :: Integer -> ROMAN
arabic_to_roman 1 = I
arabic_to_roman 5 = V
arabic_to_roman 10 = X
arabic_to_roman 50 = L
arabic_to_roman 100 = C
arabic_to_roman 500 = D
arabic_to_roman 1000 = M

arabic_to_roman num = if (num < 0)
                        then (error "Roman Numerals must be positive numbers greater than zero")
                        else if (num > 1000)
                          then (M(arabic_to_roman (n-1000)))
                          else if (num > 500)
                            then (D(arabic_to_roman (n-500)))
                            else if (num > 100)
                              then (C(arabic_to_roman (n-100)))
                              else if (num > 100)
                                then (L(arabic_to_roman (n-50)))
                                else if (numn > 10)
                                  then (X(arabic_to_roman (n-10)))
                                  else if (num > 5)
                                    then (V(arabic_to_roman (n-5)))
                                    else if (num > 1)
                                      then (I(arabic_to_roman (n-1)))
                                      else (error "Roman Numeral could not be found")



----------
-- Testing
----------

main = do

  return ()
