toDigits:: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n > 0     = (toDigits ((n - (n `mod` 10)) `div` 10)) ++ [n `mod` 10]
  | otherwise = []

rev:: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

toDigitsRev:: Integer -> [Integer]
toDigitsRev n = rev (toDigits n)

flatten:: [[a]] -> [a]
flatten [] = []
flatten (x:y) = x ++ (flatten y)

doubleOddIndices:: [Integer] -> [Integer]
doubleOddIndices [] = []
doubleOddIndices (x:[]) = [x]
doubleOddIndices (x:(y:z)) = x : ((2 * y) : doubleOddIndices(z))

doubleEveryOther:: [Integer] -> [Integer]
doubleEveryOther ls = rev (doubleOddIndices (rev ls))

allDigits:: [Integer] -> [Integer]
allDigits x = flatten (map toDigits x)

sumDigits:: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits(xs)

validate:: Integer -> Bool
validate n 
  | sumDigits (allDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0 = True
  | otherwise = False
