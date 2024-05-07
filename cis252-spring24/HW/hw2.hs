import Distribution.Simple.Utils (xargs)
-- name: Daniel Canhedo
-- SUID: 592676510

-- q1
-- return the smaller of x and y
smallerOfTwo :: Int -> Int -> Int
smallerOfTwo x y =
    if x > y
    then y
    else x

-- q2
-- return the shorter of s1 and s2
shorterOfTwo :: [Char] -> [Char] -> [Char]
shorterOfTwo s1 s2 = if length s1 < length s2
                        then s1
                        else s2

-- q3
-- return True if x is not equal to y
notEqual :: Int -> Int -> Bool
notEqual x y = not (x == y)


--q4
-- rename mystery so its name shows what it does
notAllEqual :: Int -> Int -> Int -> Bool
notAllEqual x y z = not ((x==y) && (y==z))

--q5
-- if s1 has more than 3 chars, return "---"
-- if s1 has 3 chars, return s1
-- if s1 has 2 chars return a space followed by s1
-- if s1 has 1 char, return two spaces followed by s1
-- if s1 is empty, return three spaces
rightJustify :: [Char] -> [Char]
rightJustify s1
    | (length s1 > 3) = "---"
    | (length s1 == 3) = s1
    | (length s1 == 2) = " " ++ s1
    | (length s1 == 1) = "  " ++ s1
    | otherwise = "   "

--q6
isVowel :: [Char] -> Bool
isVowel s1
    | (s1 == "a") || (s1 == "A")= True
    | (s1 == "e") || (s1 == "E") = True
    | (s1 == "i") || (s1 == "I") = True
    | (s1 == "o") || (s1 == "O") = True
    | (s1 == "u") || (s1 == "U") = True
    | otherwise = False

--q7
-- if x is a multiple of 3 and 5, return "fizzbuzz"
-- if x is a multiple of 3, return "fizz"
-- if x is a multiple of 5, return "buzz"
-- if x is not a multiple of 3 or 5, return ""
fizzbuzz :: Int -> [Char]
fizzbuzz x
    | (((x `mod` 3) == 0) && ((x `mod` 5) == 0)) = "fizzbuzz"
    | ((x `mod` 3) == 0) = "fizz"
    | ((x `mod` 5) == 0) = "buzz"
    | otherwise = ""


--q8
-- assume that s1 and s2 both have at least one char
-- return the first char of s1 followed by upto first five chars of s2
getLogin :: [Char] -> [Char] -> [Char]
getLogin s1 s2 = (head s1) : (take 5 s2)

--q9
-- assume that s1 and s2 both have at least one char
-- return the first char of s1 followed by upto first five chars of s2
-- if return string is less than six chars, append 0s
getPaddedLogin :: [Char] -> [Char] -> [Char]
getPaddedLogin s1 s2
    | length (head s1 : take 5 s2) < 6 = (head s1 : (take 5 s2)) ++ take (6 - (length (head s1 : take 5 s2))) "000000000" -- if length of string < 6 then string + take num zero required from "00000"
    | otherwise = head s1 : take 5 s2


--q10
-- if all three params are equal, return one of them
-- if two params are equal, return one of the two
-- if none of the three are equal, return the middle value
-- cases
-- 1 2 3
-- 1 3 2
-- 2 1 3
-- 2 3 1
-- 3 1 2
-- 3 2 1
middleOfThree :: Int -> Int -> Int -> Int
middleOfThree x y z
    | (x == y) && (y == z) = x
    | (x == y) = x
    | (x == z) = x
    | (y == z) = y
    | ((x > y) && (x < z)) || (x > z) && (x < y) = x
    | ((y > x) && (y < z)) || (y > z) && (x < y) = y
    | (z > x) && (z < y) = z
    | otherwise = z


