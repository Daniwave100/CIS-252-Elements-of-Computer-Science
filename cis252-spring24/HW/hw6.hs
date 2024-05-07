-- name: Daniel Canhedo
-- SUID: 592676510

-- placeholder function so file compiles
-- like 'pass' in Python labs

yourCodeGoesHere = error "todo"

-- q1
-- Implement doubleAll using a list comprehension.
-- For example, if lst1 = [10, 2, 3, 9]
-- doubleAll lst1 should return [20, 4, 6, 18]
doubleAll :: [Int] -> [Int]
doubleAll lst1 = [x * 2 | x <- lst1]

-- q2
-- Implement matches using a list comprehension.
-- For example, if lst1 = [1, 2, 1, 4, 5, 1]
-- matches 1 lst1 should return [1, 1, 1]
-- matches 2 lst1 should return [2]
-- matches 3 lst1 should return []
matches :: Int -> [Int] -> [Int]
matches n1 lst1 = [x | x <- lst1, x == n1]

-- q3
-- If s1 ends with s2, return True. Otherwise return False.
-- For example, is s1 = "hello" and s2 = "llo" return True.
-- For example, is s1 = "qwerty" and s2 = "try" return False.
s1EndsWiths2 :: String -> String -> Bool
s1EndsWiths2 s1 s2
                | length s2 > length s1 = False
                | otherwise = s2 == drop (length s1 - length s2) s1

-- q4
-- Return the length of the longest word in lst1 using a list comprehension.
-- For example, if lst1=["four", "one", "nineteen", "eight"]
-- return 8
lengthOfLongestWord :: [String] -> Int
lengthOfLongestWord lst1 = maximum [length x | x <- lst1]

-- q5
-- Return the longest word in lst1 using a list comprehension.
-- For example, if lst1=["four", "one", "nineteen", "eight"]
-- return "nineteen"
longestWord :: [String] -> String
longestWord lst1 = head (filter (\x -> (length x) == maxLength) lst1)
                    where maxLength = maximum [length x | x <- lst1]
--longestWord lst1 = head [x | x <- lst1, length x == lengthOfLongestWord lst1]



-- q6
-- Q6.2 from textbook.
-- Return the section of lst1 from index beg to end.
-- The item at index beg should be included in the result
-- but the item at end should not be included.
-- From last semester's terminology, beg is inclusive
-- and end is exclusive.
-- If beg<0 or end<0 or end<beg, return the empty list.
-- For example: subseq 2 5 [1 .. 10] should return [3, 4, 5]
subseq :: Int -> Int -> [Int] -> [Int]
subseq beg end lst1
    | beg < 0 = []
    | end < 0 = []
    | end < beg = []
    | otherwise = [x | x <- drop beg (take end lst1)] -- question about inclusive/exclusive


-- q7

-- Implement booksBorrowed using a list comprehension.
-- Assume library is a list of String tuples.
-- Each tuple has a user name, and the title of a book
-- that user has borrowed.
-- Given a user name and library, return all the books
-- that user has borrowed.
-- For example, if library = [("Alice", "Tintin"), ("Anna", "Little Women"), ("Alice", "Asterix"), ("Rory", "Tintin")]
-- and user = "Alice" return ["Tintin", "Asterix"].
-- With the same library, and user = "Jane" return [].
booksBorrowed :: String -> [(String, String)] -> [String]
booksBorrowed user library = [x | (y, x) <- library, y == user]

-- q8

-- Implement booksCurrentlyBorrowed using a list comprehension.
-- Now assume that library is a list of String tuples, but
-- each tuple has a user name, the title of a book, and a boolean
-- representing if the book has been returned.
-- For example, library might be:
-- [("Alice", "Tintin", False),
--  ("Anna", "Little Women", True),
--  ("Alice", "Asterix", True),
--  ("Rory", "Tintin", False)]
-- So Alice borrowed two books, Tintin and Asterix, but she still
-- has Tintin while she has already returned Asterix.
-- For example, if booksCurrentlyBorrowed is called with the
-- library shown and user = "Alice" return ["Tintin"].
-- If user = "Anna" return [].
-- If user = "Rory" return ["Tintin"].
booksCurrentlyBorrowed :: String -> [(String, String, Bool)] -> [String]
booksCurrentlyBorrowed user library = [b |(a, b, c) <- library, (a == user) && (c == False)]

-- q9

-- Implement borrowers using a list comprehension.
-- With the same library as above, return all users who have
-- a book, or books, currently checked out.
-- If library is as above, return ["Alice", "Rory"]
--
-- Another example, if library is:
-- [("Alice", "Tintin", False),
--  ("Anna", "Little Women", True),
--  ("Alice", "Asterix", True),
--  ("Alice", "Haskell", False),
--  ("Rory", "Tintin", False)]
-- borrowers should return ["Alice", "Alice", "Rory"].
-- In other words, its fine for the returned list to have
-- duplicates.
borrowers :: [(String, String, Bool)] -> [String]
borrowers library = [a | (a, b, c) <- library, c == False]

-- q10

-- Implement numBorrowed using a list comprehension.
-- If library is:
-- [("Alice", "Tintin", False),
--  ("Anna", "Little Women", True),
--  ("Alice", "Asterix", True),
--  ("Alice", "Haskell", False),
--  ("Rory", "Tintin", False)]
--  and user is "Alice" return 2
numBorrowed :: String -> [(String, String, Bool)] -> Int
numBorrowed user library = length [a | (a, b, c) <- library, (a == user) && (c == False)]
                            


