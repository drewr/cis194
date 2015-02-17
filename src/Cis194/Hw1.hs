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

-- Buggy, still generates too many moves
hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 0 _ _ _  _  = []
hanoi2 1 a b _  _  = [(a, b)]
hanoi2 2 a b t1 _  = [(a, t1), (a, b), (t1, b)]
hanoi2 n a b t1 t2 = concat [(hanoi2 (n - 2) a t2 t1 b),
                             (hanoi2 1 a t1 b t2),
                             (hanoi2 1 a b t1 t2),
                             (hanoi2 1 t1 b a t2),
                             (hanoi2 (n - 2) t2 b t1 a)]
