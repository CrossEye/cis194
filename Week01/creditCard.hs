toDigitsRev:: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = n `mod` 10 : toDigitsRev ((n - (n `mod` 10)) `div` 10)

rev:: [Integer] -> [Integer] -- obviously could be more generic
rev xs = rev' xs []  where
  rev' (x:xs) acc = rev' xs (x:acc)
  rev' []     acc = acc

toDigits:: Integer -> [Integer]
toDigits n = rev (toDigitsRev n)