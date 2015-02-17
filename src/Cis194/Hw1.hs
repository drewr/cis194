module Cis194.Hw1 where

import Data.Char (digitToInt)

main = putStrLn . show . toDigitsRev

validate :: Integer -> Bool
validate n = rem (sumDigits . doubleEveryOther . toDigits $ n) 10 == 0

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = (map (toInteger . digitToInt)) . show $ n

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleFromLeft :: [Integer] -> [Integer]
doubleFromLeft [] = []
doubleFromLeft [x] = [x]
doubleFromLeft (x:[y]) = x : [(y * 2)]
doubleFromLeft (x:y:zs) = x : (y * 2) : (doubleFromLeft zs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleFromLeft . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

-----------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = concat [(hanoi (n - 1) a c b), (hanoi 1 a b c), (hanoi (n - 1) c b a)]
