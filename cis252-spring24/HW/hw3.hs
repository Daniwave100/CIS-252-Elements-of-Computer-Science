
    -- name: Daniel Canhedo
    -- SUID: 592676510

    -- placeholder function so file compiles
    -- like 'pass' in Python labs
    yourCodeGoesHere = error "todo"

    -- q1
    -- rewrite with a where clause so the expressions (mod x 3)
    -- and (mod x 5) appear only once
    -- if x is a multiple of 3 and 5, return "fizzbuzz"
    -- if x is a multiple of 3, return "fizz"
    -- if x is a multiple of 5, return "buzz"
    -- if x is not a multiple of 3 or 5, return ""
    fizzbuzz :: Int -> [Char]
    fizzbuzz x
            | (((modThree) == 0) && ((modFive) == 0)) = "fizzbuzz"
            | ((modThree) == 0) = "fizz"
            | ((modFive) == 0) = "buzz"
            | otherwise = ""
            where modThree = x `mod` 3
                  modFive = x  `mod` 5

    -- utility function
    getLogin :: [Char] -> [Char] -> [Char]
    getLogin s1 s2 = take 1 s1 ++ take 5 s2

    -- q2
    -- rewrite getPaddedLogin by adding an expression to the where clause so that 
    -- the length function is only used once
    getPaddedLogin :: [Char] -> [Char] -> [Char]
    getPaddedLogin s1 s2
                    | getFive < 6 = (head s1 : (take 5 s2)) ++ take (6 - getFive) "000000000" -- if length of string < 6 then string + take num zero required from "00000"
                    | otherwise = head s1 : take 5 s2
                    where getFive = length (head s1 : take 5 s2)

    -- q3
    -- Write a lambda function which takes one parameter and returns the first
    -- character of its parameter. 
    q3 = (\x -> head x)

    --q4
    -- Write a lambda function which takes two parameters and returns the first
    -- character of its first parameter and upto five characters of its second
    -- parameter.
    q4 = (\x y -> head x : take 5 y)

    --q5
    -- Write a lambda function which takes two parameters (x and y) and returns x
    -- if x is greater than y, and in all other cases returns y
    q5 :: Int->Int->Int
    q5 = (\x y ->   if x > y 
                    then x
                    else y)

    --q6
    -- if lst1 = [1, 2, 3, 4] return [4, 1, 2, 3]
    -- if lst1 = [1, 2, 3, 4, 5] return [5, 1, 2, 3, 4]
    -- if lst1 = [9, 3, 21, 60, 2, 6] return [6, 9, 3, 21, 60, 2]
    -- hint: consider using head/tail init/last
    rotateByOne :: [Int] -> [Int]
    rotateByOne lst1 = (last lst1) : (init lst1)

    --q7
    -- if lst1 = [1, 2, 3, 4] return [3, 4, 1, 2]
    -- if lst1 = [1, 2, 3, 4, 5] return [4, 5, 1, 2, 3]
    -- if lst1 = [9, 3, 21, 60, 2, 6] return [2, 6, 9, 3, 21, 60]
    -- hint: consider using head/tail init/last
    rotateByTwo :: [Int] -> [Int]
    rotateByTwo lst1 = two
                    where one = (last lst1) : (init lst1)
                          two = (last one) : (init one)

    --q8
    -- "radar" should return True
    -- "ranger" should return False
    -- "rotator" should return True
    -- "rotated" should return False
    -- hint: consider using head/tail init/last
    startsAndEndsWithSameTwoLetters :: [Char] -> Bool
    startsAndEndsWithSameTwoLetters lst1 = if (first2) == (last2)
                                            then True
                                            else False
                                            where first2 = take 2 lst1
                                                  last2 = take 2 (reverse lst1)
                                                    


        --q9
    -- 123 returns [2, 3]
    -- 2345 returns [4, 5]
    -- 92823 returns [2, 3]
    lastTwoDigits :: Int -> [Int]
    lastTwoDigits x = [(x `div` 10) `mod` 10, (x `mod` 10)]

    --q10
    -- Given the values of x, y and z fill in the table with values for the 4 
    -- expressions given. Then implement middleOfThree again.
    -- expr1 = max(x,y)
    -- expr2 = min(x,y)
    -- expr3 = min(max(x,y), z) ie min(expr1, z)
    -- expr4 = max(min(x,y), (min(max(x, y), z))) ie max(expr2, expr3)
    -- x y z expr1  expr2   expr3   expr4
    -- 1 2 3   2      1       2       2
    -- 1 3 2   3      1       2       2
    -- 2 1 3   2      1       2       2
    -- 2 3 1   3      2       1       2
    -- 3 1 2   3      1       2       2
    -- 3 2 1   3      2       1       2
    --q10


    middleOfThree :: Int -> Int -> Int -> Int
    middleOfThree x y z
        | (x == y) && (y == z) = x
        | (x == y) = x
        | (x == z) = x
        | (y == z) = y
        | ((x > y) && (x < z)) || ((x > z) && (x < y)) = x
        | ((y > x) && (y < z)) || ((y > z) && (x > y)) = y
        | ((z > x) && (z < y)) || ((z < x) && (z > y)) = z
        | otherwise = z




