--Data Types
data ROMAN = I | V | X
  deriving (Eq,Show) -- for equality and printing


--Converting roman numerals to arabic numerals/integers
roman_to_arabic :: ROMAN -> Integer
roman_to_arabic I =  1
roman_to_arabic V =  5
roman_to_arabic X = 10

roman_to_arabic I val = (roman_to_arabic val) + 1
roman_to_arabic V val = (roman_to_arabic val) + 5
roman_to_arabic X val = (roman_to_arabic val) + 10



--Converting arabic numerals/integers to roman numerals
arabic_to_roman :: Integer -> ROMAN
arabic_to_roman  1 = I
arabic_to_roman  5 = V
arabic_to_roman 10 = X

arabic_to_roman num = if (n < 0)
                        then (error "Roman Numerals must be positive numbers greater than zero")
                        else if (n > 10)
                          then (X (arabic_to_roman (n-10)))
                          else if (n > 5)
                            then (V (arabic_to_roman (n-5)))
                            if (n > 1)
                              then (I (arabic_to_roman (n-1)))
                                else (error "Roman Numeral could not be found")
