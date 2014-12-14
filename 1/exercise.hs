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

--
-- Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
--
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (p: ps)
  | p < 10 = (p + (sumDigits ps))
  | otherwise = (sum (toDigits p)) + (sumDigits ps)

--
-- Example: validate 4012888888881881 = True
-- Example: validate 4012888888881882 = False
--
validate :: Integer -> Bool
validate card_number = ((sumDigits (doubleEveryOther (toDigits card_number))) `mod` 10) == 0
