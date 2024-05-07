import Data.Char

-- name: Daniel Canhedo
-- SUID: 592676510

-- placeholder function so file compiles
-- like 'pass' in Python labs

yourCodeGoesHere = error "todo"

-- Write recursive implementations for all questions.

-- q1
-- Write a recursive implementation of capitalize, which takes
-- a String as input and returns a String with all chars
-- replaced by their uppercase version. Assume that the input
-- String contains a-z only.
-- Use the toUpper function from Data.Char.
capitalize :: String -> String
capitalize [] = ""
capitalize (x:xs) = toUpper x : capitalize xs

-- q2
-- Write a recursive implementation of replace1WithOne, which
-- takes a String as input and returns a String with all occurences
-- of the digit '1' replaced with the String "One".
-- Assume that the input String has only the chars a-z and 0-9.
replace1WithOne :: String -> String
replace1WithOne [] = ""
replace1WithOne (x:xs) 
                | x == '1' = "One" ++ replace1WithOne xs
                | otherwise = x : replace1WithOne xs

-- q3
-- Write a recursive implementation of replaceDigitWithWord. This is
-- a generalization of q2, so any digit should be replaced with the
-- corresponding word. The correspoinding words are in the list subs,
-- in the correct position. So subs !! 0 is "Zero", subs !! 1 is "One"
-- and so on.
-- Assume that subs will always be correct, and s1 will only have the
-- chars a-z and 0-9.
-- For example: if  s1 = "hello1there2" and
-- subs = ["Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"]
-- return "helloOnethereTwo"
-- The functions isDigit and digitToInt from Data.Char will be useful.
replaceDigitWithWord :: String -> [String] -> String
replaceDigitWithWord [] subs = ""
replaceDigitWithWord (x:xs) subs 
                            | isDigit x = (subs !! digitToInt x) ++ replaceDigitWithWord xs subs
                            | otherwise = x : replaceDigitWithWord xs subs

-- q4
-- Write a recursive implementation of allCapsAfterUnderscore. Assume that
-- the input string will have either 0 or 1 underscore '_' char.
-- For example, if s1 = "helloworld" return "helloworld".
-- For example, if s1 = "hello_world" return "hello_WORLD".
-- Hint: the capitalize function from q1 could be useful.
allCapsAfterUnderscore :: String -> String
allCapsAfterUnderscore [] = ""
allCapsAfterUnderscore (x:xs) 
                        | x == '_' = x : capitalize xs
                        | otherwise = x : allCapsAfterUnderscore xs

-- q5
-- Write a recursive implementation of insertAtPos which returns a list
-- with s1 at pos n.
-- For example, if n=0 s1="hello" and lst1 = ["ta", "da"] then return
-- ["hello", "ta", "da"]
-- For example, if n=1 s1="hello" and lst1 = ["ta", "da"] then return
-- ["ta", "hello", "da"]
-- Assume that n will always be between 0 (inclusive) and length lst1
-- (exclusive). If lst1 has 2 items, n will be either 0 or 1. If lst1 has
-- four items, n will be 0, 1, or 2.
insertAtPos :: Int -> [String] -> String -> [String]
insertAtPos n (x:xs) s1 
                    | n > 0 = x : insertAtPos (n-1) xs s1
                    | otherwise = s1: x : xs
-- q6
-- Write a recursive implemenation of alternate, which produces one list
-- from alternate items from its two inputs.
-- For example, if lst1 = [1, 2, 3] and lst2 = [10, 11, 12] return
-- [1, 10, 2, 11, 3, 12]
-- If the input lists are not the same length, include all the elements
-- from the longer list at the end.
-- For example, if lst1 = [1, 2, 3] and lst2 = [10, 11, 12, 13, 14] return
-- [1, 10, 2, 11, 3, 12, 13, 14].
-- For example, if lst1 = [1, 2, 3, 4, 5] and lst2 = [10, 11, 12] return
-- [1, 10, 2, 11, 3, 12, 4, 5].
alternate :: [Int] -> [Int] -> [Int]
alternate [] ys = ys
alternate xs [] = xs
alternate (x:xs) (y:ys) = x : y : alternate xs ys

-- q7
-- Write a recursive implementation of myZip, which returns a list of
-- tuples, with each tuple having one element from each input list. If
-- the two lists are not equal length, discard the extra elements.
-- For example, if lst1 = [1, 2, 3] and lst2 = [10, 11, 12] return
-- [(1, 10), (2, 11), (3, 12)]
-- For example, if lst1 = [1, 2, 3] and lst2 = [10, 11, 12, 13, 14] return
-- [(1, 10), (2, 11), (3, 12)]
-- For example, if lst1 = [1, 2, 3, 4, 5] and lst2 = [10, 11, 12] return
-- [(1, 10), (2, 11), (3, 12)]
myZip :: [Int] -> [Int] -> [(Int, Int)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- q8
-- Insert x into the sorted lst1 so the result is still sorted.
-- For example, if x=1 and lst1 = [2, 7, 10], return [1, 2, 7, 10]
-- For example, if x=3 and lst1 = [2, 7, 10], return [2, 3, 7, 10]
-- For example, if x=9 and lst1 = [2, 7, 10], return [2, 7, 9, 10]
-- For example, if x=21 and lst1 = [2, 7, 10], return [2, 7, 10, 21]
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) 
            | x > y = y : insert x ys
            | otherwise = x : y : ys

-- q9
-- Write a recursive implementation of merge, which assumes that both
-- its input lists are sorted, and combines them into one sorted list.
-- For example, if lst1 = [1, 4, 10] and lst2 = [0, 2, 7, 11, 12]
-- merge would return [0, 1, 2, 4, 7, 10, 11, 12]
--
-- If either input list is empty, just return the other list.
--
-- If neither input list is empty, compare the first element from
-- both lists. Using the example above, since 0 is less than 1, it
-- should go first. So, 0 should be added to result, and consed with
-- a recursive call to merge using (tail lst1) and lst2 as params.
--
-- There is a good solution which just uses recursive calls to merge.
-- In other words, you don't have to use any other functions from
-- this homework, or Data.Char etc.
merge :: [Int] -> [Int] -> [Int]
merge [] lst2 = lst2
merge lst1 [] = lst1
merge (x:xs) (y:ys)
                | x <= y = x : merge xs (y:ys)
                | otherwise = y : merge (x:xs) ys

-- q10
-- Insertion sort is an algorithm which builds up a sorted list from      read description on wiki. each step, take first item off of list and then insert it into whats left. If i split list into 7 and then 1, 2, 10, then i have to insert 7 into 1, 2, 10 which is a problem we already solved. call insert from back there.
-- an unsorted input list.
--
-- From https://en.wikipedia.org/wiki/Insertion_sort:
-- At each step, the algorithm removes one element from the input data
-- finds the location it belongs within the sorted list, and inserts it
-- there. It repeats until no input elements remain.
--
-- For example, if lst1 = [7, 1, 2, 10] the algorithm would start by
-- making an empty list, say result = [].
-- Next take the first element from lst1 and insert it into the correct
-- position in result. So lst1 is now [1, 2, 10] and result = [7].
-- Now repeat for 1, so lst1 = [2, 10] and result = [1, 7]
-- Repeat again for 2, so lst1 = [10] and result = [1, 2, 7]
-- Lastly, put 10 in the right place, so lst1 = [] and result = [1, 2, 7. 10]
--
-- Write a recursive implementation of insertion sort. There is a good
-- solution using recursive calls to insert (from q8) and iSort.
iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)
