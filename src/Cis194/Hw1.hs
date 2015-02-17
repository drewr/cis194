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
