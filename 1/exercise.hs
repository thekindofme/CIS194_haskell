lastDigit :: Integer -> Integer
lastDigit 0 = 0
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

toDigits :: Integer -> [Integer]
toDigits x
| x < 1 = []
| otherwise = toDigits(dropLastDigit(x)) ++ [lastDigit(x)]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (p:ps)
| (length(ps) `mod` 2 == 0) = p : doubleEveryOther ps
| otherwise = (p*2) : doubleEveryOther ps
