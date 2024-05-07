import Data.Char

-- name: Daniel Canhedo
-- SUID: 592676510

-- placeholder function so file compiles
-- like 'pass' in Python labs

yourCodeGoesHere = error "todo"

-- Write recursive implementations for questions 1-4.

-- q1
-- A palindrome is a word that doesn't change if reversed. For example, "radar"
-- "racecar", "noon" or "hannah".
-- https://en.wikipedia.org/wiki/Palindrome
-- Notice that if the word has an even number of letters, each letter occurs
-- an even number of times. So "noon" has 2 'n's and 2 'o's.
-- If the word has an odd number of letters, at least one letter occurs an
-- odd number of times. So "radar" has just one 'd'.
-- Write a recursive implementation of isPalindrome which takes a String as
-- input and returns True if the input is a palindrome.
-- You should probably consider two base cases, one when the input String is
-- empty and another when it has only one character.

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome val 
                | (head val == last val) = isPalindrome (init (tail val))
                | otherwise = False

-- q2
-- Write a recursive implementation of longestCommonPrefix, which takes two
-- input Strings and returns the longest prefix that both have in common.
-- For example, if s1="flow" and s2="flower" the function would return "flow".
-- If s1="flight" and s2="flow" the function would return "fl".
-- If s1="some" and s2="else" the function would return "".
longestCommonPrefix :: String -> String -> String
longestCommonPrefix s1 [] = []
longestCommonPrefix [] s2 = []
longestCommonPrefix (x:xs) (y:ys) 
                    | x == y = x : longestCommonPrefix xs ys
                    | otherwise = ""

-- q3
-- Write a recursive implementation for consecutiveCharPrefix, which takes a
-- String input and returns an Int representing how many times the first char
-- is repeated in input.
-- For example, if s1="asdf" the function would return 1.
-- If s1="aasdf" the function would return 2.
-- If s1="qqqwe" the function would return 3.
-- If s1 = "" the function would return 0.
consecutiveCharPrefix :: String -> Int
consecutiveCharPrefix [] = 0
consecutiveCharPrefix [_] = 1
consecutiveCharPrefix (x:y:xs)
                    | x == y = 1 + consecutiveCharPrefix (y:xs)
                    | otherwise = 1

consecutiveCharPrefix (x:xs) = if x == head xs
                                then 1 + consecutiveCharPrefix xs
                                else 1



-- q4
-- This the hardest problem on this set!
-- Write a recursive implementation for compress, which takes a String input
-- and returns a String with consecutive repeated chars replaced with one
-- char and a count. If the count is 1, don't include it in the returned string.
-- No char in s1 will be repeated more than 9 times.
-- For example, is s1="aabbcc" return "a2b2c2".
-- If s1 = "abbbcc" return "ab3c2".
-- If s1 = "abbbbb" return "ab5".
-- If s1 = "a" return "a".
-- If s1 = "" return "".
-- Hint: consider using your solution for q3, and intToDigit from Data.Char. -- think about how you can use consecutiveCharPrefix to solve this problem (drop) USE DROP

compress :: String -> String
compress [] = ""
compress (x:xs) = x : (if consecutiveCharPrefix (x:xs) > 1 
                        then ([intToDigit (consecutiveCharPrefix (x:xs))])
                        else "") ++ compress (drop (consecutiveCharPrefix (x:xs) - 1) xs)

compress s1 = if count == 1
                then head s1 : compress (tail s1)
                else head s1 : (intToDigit count) : compress (drop count s1)
                where count = consecutiveCharPrefix s1
            

-- q5
-- Give the type declaration for a function addItemToList which takes two
-- parameters, the second is a list. It inserts its first param into its
-- second param, using logic too complicated to describe.

addItemToList :: a -> [a] -> [a]
addItemToList item list = yourCodeGoesHere

-- :: a -> [a] -> [a]

-- q6
-- Give the type declaration for a function, combineTwoLists, which takes two
-- list params and returns a combination of them. All the items from both 
-- parameter lists are included in the returned list.

combineTwoLists :: [a] -> [a] -> [a]
combineTwoLists list1 list2 = yourCodeGoesHere

-- :: [a] -> [a] -> [a]

-- q7
-- Give the type declaration for a function, lengthOfNestedLists, which takes
-- a 2D list as parameter. It uses the Prelude length function to compute the
-- length of each nested list, and returns a list of lengths.

lengthOfNestedLists :: [[a]] -> [Int]
lengthOfNestedLists [] = []
lengthOfNestedLists (x:xs) = yourCodeGoesHere

-- :: [[a]] -> [Int]

-- q8
-- Give the type declaration of f8 which takes one param, of any type, and
-- returns a value of the same type.

f8 :: a -> a 
f8 a = yourCodeGoesHere

-- :: a -> a

-- q9
-- Give the type declaration for the higher-order function, transformList,
-- which takes two params. Its first param is a function, which has a type
-- declaration like f8 from q8 above. Its second param is a list. It applies
-- its first param to each item in its second param, and returns a list of
-- the resulting values.

transformList :: (a -> a) -> [a] -> [a]
transformList func list = yourCodeGoesHere

-- :: (a -> a) -> [a] -> [a]

-- q10
-- Give the type declaration for the higher-order function, accordion,
-- which takes three params. Its first param is a function, which has a type
-- declaration like f8 from q8 above. Its second param is a value, which
-- can be of any type. The third param is a list of any type. The return
-- value of accordion is computer by repeatedly applying its first param
-- to an item from its third param, and storing the result in its second
-- param.

accordian :: (a -> a) -> b -> [c] -> b
accordian func val = yourCodeGoesHere

-- :: (a -> a) -> b -> [c] -> b