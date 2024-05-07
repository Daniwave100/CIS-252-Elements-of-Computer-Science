-- name: Daniel Canhedo
-- SUID: 592676510

-- placeholder function so file compiles
-- like 'pass' in Python labs
yourCodeGoesHere = error "todo"

-- q1
-- Which of v1 and v2 is a correct implementation of the compound
-- function  (twice . decr) ?
decr :: Int -> Int
decr x = (\a -> a-1) x

twice :: Int -> Int
twice x = (\b -> b*2) x

v1 = (\x -> (\y -> y-1) x * 2 ) -- v1 is the correct implementation of the compound function (twice . decr)
v2 = (\x -> (\y -> y*2) x - 1 )

-- q2
-- Which of v3 and v4 is a correct implementation of the compound
-- function  (decr . twice) ?
v3 = (\x -> ((\y -> (y-1)) x) * 2 )
v4 = (\x -> ((\y -> (y*2)) x) - 1 ) -- v4 is the correct implementation of the compound function (decr . twice)

-- q3
-- What is the result of calling mySubtract with just one parameter?
-- In other words, what's the type of v5?
mySubtract :: Int -> Int -> Int
mySubtract x y = (\a -> (\b -> a-b)) x y

v5 = mySubtract 10 -- v5 is technically a function which takes an integer parameter and returns an integer parameter. v5 :: Int -> Int

-- q4
-- Rewrite myProduct as a nested lambda function.
myProduct :: Int -> Int -> Int
myProduct x y = x * y

myProduct' :: Int -> Int -> Int
myProduct' x y = (\a -> (\b -> a * b)) x y

-- q5
-- The function serialDivision is supposed to return the result of
-- x // y // z, where // is integer division. So for example, if
-- serialDivision was called with x=100, y=10 and z=2, it should
-- return 5.
-- Find and fix the bug in serialDivision.
serialDivision :: Int -> Int -> Int -> Int
serialDivision x y z = (\a -> (\b -> (\c -> div (div a b) c))) x y z

-- q6
-- Rewrite someArithmetic as a nested lambda function.
someArithmetic :: Int -> Int -> Int -> Int
someArithmetic x y z = 2 * x + y - z

someArithmetic' :: Int -> Int -> Int -> Int
someArithmetic' x y z = (\a -> (\b -> (\c -> 2 * a + b - c))) x y z

--q7
-- Replace the call to f1 in v6 with a lambda function which returns the
-- same result as f1.
-- v6 should equal [1,4,2,3,3,2,0]
lst1 = [1, 9, 2, 8, 3, 2, 0]

f1 :: Int -> Int
f1 x = if x > 5 then mod x 5 else x

v6 = map f1 lst1
v6' = map (\x -> if x > 5 then mod x 5 else x) lst1

--q8
-- Rewrite the expression for v7 so that the call to f1 is replaced with
-- a lambda function.
v7 = [f1 x | x <- lst1]
v7' = [(\x -> if x > 5 then mod x 5 else x) x | x <- lst1]

-- q9
-- Rewrite the expression for v8 so that the call to f2 is replaced with
-- a lambda function.
f2 :: Int -> Bool
f2 x = mod (x-1) 3 == 0

v8 = filter f2 lst1
v8' = filter (\x -> mod (x - 1) 3 == 0) lst1

-- q10
-- Rewrite the expression for v9 so that the call to f2 is replaced with
-- a lambda function.
v9 = [x | x <- lst1, f2 x]
v9' = [x | x <- lst1, (\x -> mod (x - 1) 3 == 0) x]
