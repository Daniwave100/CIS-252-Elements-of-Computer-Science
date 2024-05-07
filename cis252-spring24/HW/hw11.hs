import Data.Char
import Test.HUnit 

-- name: Daniel Canhedo
-- SUID: 592676510

-- placeholder function so file compiles
-- like 'pass' in Python labs

yourCodeGoesHere = error "todo"


-- q1
-- Write a recursive implementation of indexOf which looks for a String
-- in a [String] and returns the index of String, or -1 if the list doesn't
-- contain the String.
-- For example, "a" ["a", "b", "c", "d"] returns 0
-- For example, "c" ["a", "b", "c", "d"] returns 2
-- For example, "e" ["a", "b", "c", "d"] returns -1

indexOf :: String -> [String] -> Int
indexOf _ [] = -1
indexOf s1 (x:xs) 
            | s1 == x = 0
            | otherwise = let index = indexOf s1 xs
                            in if index == -1 then  -1 else index + 1




-- q2
-- The following code defines a new data type, makes it an instance of
-- the Show class, and makes one object of the new data type.
-- The data type relies on some type synonyms, which are missing.
-- Write the type synonyms so this code compiles.

type SUID = Int
type Score = Int

data LabScore = LabScore SUID Score

instance Show LabScore where
  show (LabScore suid score) = show suid ++ ": " ++ show score

ls10 = LabScore 123456789 89




-- q3
-- Review section 4.1.2 from the textbook.
-- Write an implementation for compare, which takes two LabScore objects,
-- ls1 and ls2, and returns LT if the SUID of ls1 is less than the SUID of
-- ls2, EQ if the two SUIDs are equal, and GT otherwise.
compareLabScores :: LabScore -> LabScore -> Ordering
compareLabScores (LabScore suid1 _) (LabScore suid2 _)
                    | (suid1) < (suid2) = LT
                    | (suid1) == (suid2) = EQ
                    | otherwise = GT





-- q4
-- The code shown should increment the second item in each tuple of a list.
-- For example, if lst5 = [(1, 2), (10, 11), (20, 21)] result5 should be
-- [(1, 3), (10, 12), (20, 22)].
-- But it doesn't work. Find and fix the bug.

lst5 = [(1, 2), (10, 11), (20, 21)]

result5 = map (\(x,y)-> (x, y+1)) lst5




-- q5
-- Write a lambda function so that the code shown increments the score
-- for each LabScore in a list.
-- For example:
ls1 = LabScore 999 89
ls2 = LabScore 123 90
ls3 = LabScore 444 99
-- and
lst6 = [ls1, ls2, ls3]
-- then result6 should be [LabScore 999 90, LabScore 123 91, LabScore 444 100]

result6 = map (\(LabScore x y) -> LabScore x (y+1)) lst6




-- q6
-- Write a lambda function so that the code shown returns all the LabScores
-- for a given suid from a list.
-- For example:
ls4 = LabScore 999 34
lst7 = [ls1, ls2, ls3, ls4]
-- and
targetSUID = 999
-- then result7 should be [LabScore 999 89, LabScore 999 34]
result7 = filter (\(LabScore x y) -> x == targetSUID) lst7




-- q7
-- Write an expression using foldl to calculate the total lab score
-- for targetSUID.
-- For example, using lst7 and targetSUID from q7, result8 should be 89+34 = 123

result8 = foldl (\acc (LabScore x y) -> if x == targetSUID then acc + y else acc) 0 lst7




-- q8
-- Write a recursive implementation for sumOfScores, which takes [LabScore]
-- and returns the sum of all the scores.
-- For example, using lst7 as param, the result should be
-- 89 + 90 + 99 + 34 = 312
sumOfScores :: [LabScore] -> Int
sumOfScores [] = 0
sumOfScores ((LabScore x y):xs) = y + sumOfScores xs


-- q9
-- Write tests for the function allEqual, which should return True if all
-- three of its params are equal. The implementation of allEqual is not
-- given, so this is an exercise in black box testing.

allEqual :: Int -> Int -> Int -> Bool
allEqual i1 i2 i3 = error "no impl given"
test1 = TestCase (assertEqual "All are equal" True (allEqual 3 3 3))
test2 = TestCase (assertEqual "i1 is not equal" False (allEqual 1 3 3))
test3 = TestCase (assertEqual "i2 is not equal" False (allEqual 3 2 3))
test4 = TestCase (assertEqual "i3 is not equal" False (allEqual 3 3 1))
test5 = TestCase (assertEqual "None are equal" False (allEqual 1 2 3))
allTests1 = TestList [test1, test2, test3, test4, test5]

-- q10
-- Write tests for the function allDifferent which should return False if
-- any two (or more) of its params are equal. A good set of tests would
-- contain one or more failing tests, showing that the impl given is not
-- optimal.

allDifferent :: Int -> Int -> Int -> Bool
allDifferent p1 p2 p3 = (p1 /= p2) && (p2 /= p3)

t1 = TestCase (assertEqual "All params equal" False (allDifferent 10 10 10))
t2 = TestCase (assertEqual "Two params equal" False (allDifferent 10 10 5))
t3 = TestCase (assertEqual "Two params equal" False (allDifferent 5 10 10))
t4 = TestCase (assertEqual "Two params equal" False (allDifferent 10 5 10))
t5 = TestCase (assertEqual "No params equal" True (allDifferent 5 10 20))

allTests2 = TestList [t1, t2, t3, t4, t5]
