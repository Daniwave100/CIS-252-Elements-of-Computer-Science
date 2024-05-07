import Data.Char

-- name: Daniel Canhedo
-- SUID: 592676510

-- placeholder function so file compiles
-- like 'pass' in Python labs

yourCodeGoesHere = error "todo"

-- Write recursive implementations for all questions.

-- q1
-- Given n, return n + (n-1) + (n-2) ... 1
-- For example, if n=5, return 5 + 4 + 3 + 2 + 1 = 15
sumOfInts :: Int -> Int
sumOfInts 0 = 0
sumOfInts n = n + sumOfInts (n-1)

-- q2
-- Given s1, return the reverse of s1
-- Recall rotateByOne s1 = tail s1 ++ [head s1]
reverseString :: String -> String
reverseString [] = ""
reverseString s1 = reverseString (tail s1) ++ [head s1]

-- q3
-- Given s1 return a string which has all the chars from s1
-- except for digits.
-- For example, s1 = "hello9" returns "hello"
-- For example, s1 = "a1s2d3f4" returns "asdf"
filterOutDigits :: String -> String
filterOutDigits s1 = [x | x <- s1, not (elem x ['0'..'9'])]

-- q4
-- Return the sum of all integers started from beg (inclusive)
-- and up to end (exclusive).
-- For example, sumOfRange 2 5 should return 2 + 3 + 4 = 9
sumOfRange :: Int -> Int -> Int
sumOfRange beg end = sum [beg..(end-1)]

-- q5
-- Write a recursive implementation of Prelude.elem.
listContains :: [Int] -> Int -> Bool
listContains [] i1 = False
listContains lst1 i1 = if i1 == head lst1
                        then True
                        else listContains (tail lst1) i1

-- q6
-- Write a recursive implementation of Prelude.take.
-- Ignore negative values of i1.
takeFromList :: Int -> [Int] -> [Int]
takeFromList i1 [] = []
takeFromList i1 lst1 | i1 <= 0 = [] 
takeFromList i1 (x:xs) = x : takeFromList (i1-1) xs



-- q7
-- Write a recursive implementation for sumDigitChars.
-- The function digitToInt in Data.Char will be useful.
-- For example, if s1 = "hello9" return 9.
-- For example, if s1 = "a1s2d3f4" return 1+2+3+4=10.
sumDigitChars :: String -> Int
sumDigitChars [] = 0
sumDigitChars (x:xs) 
                | elem x ['0'..'9'] = digitToInt x + sumDigitChars xs
                | otherwise = sumDigitChars xs

-- q8
-- Write a recursive implementation of sumOfIntDigits.
-- Assume i1 > 0.
-- For example, if i1 = 124 return 1 + 2 + 4 = 7
-- For example, if i1 = 8971 return 8 + 9 + 7 + 1 = 25
sumOfIntDigits :: Int -> Int
sumOfIntDigits 0 = 0
sumOfIntDigits i1 = (i1 `mod` 10) + sumOfIntDigits (i1 `div` 10)

-- q9
-- Return the min item from a [Int]
-- Check that your implementation gives the same answer as Prelude.minimum
recursiveMin :: [Int] -> Int
recursiveMin [] = error "empty list"
recursiveMin [x] = x
recursiveMin (x:xs) = min x (recursiveMin xs)

-- q10
-- Write a recursive implementation of s1ContainsS2.
-- If all the chars of s2 are in s1, return True. Otherwise return False.
-- For example, s1 = "hello" s2 = "helo" returns True.
-- For example, s1 = "hello" s2 = "helop" returns False.
s1ContainsS2 :: String -> String -> Bool
s1ContainsS2 s1 [] = True
s1ContainsS2 [] s2 = False
s1ContainsS2 s1 (x:xs)
                | elem x s1 = s1ContainsS2 s1 xs
                | otherwise = False

