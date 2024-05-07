

    -- name: Daniel Canhedo
    -- SUID: 592676510

    -- placeholder function so file compiles
    -- like 'pass' in Python labs
    yourCodeGoesHere = error "todo"

    -- q1
    -- Rewrite the expression for y using a lambda function
    -- instead of the call to square.
    square :: Int -> Int
    square x = x * x
    y = square 9
    y'= (\sq -> sq * sq) 9

    -- q2
    -- Rewrite the expression for z using a lambda function
    -- instead of the call to myProduct.
    myProduct :: Int -> Int -> Int
    myProduct x y = x * y
    z = myProduct 8 4
    z'= (\x y -> x * y) 8 4

    -- q3
    -- Rewrite the expression for result using a lambda function
    -- instead of the call to longerList.
    longerList :: [Int] -> [Int] -> [Int]
    longerList lst1 lst2 = if length lst1 > length lst2
                           then lst1
                           else lst2
    input1 = [1, 2, 3, 4]
    input2 = [4, 3, 2]
    result = longerList input1 input2


    result' :: [Int] -> [Int] -> [Int]
    result' = (\inp1 inp2 -> if (length inp1) > (length inp2)
                            then inp1
                            else inp2)

    -- q4
    -- Rewrite squareOfProduct to use a lambda function instead of
    -- the function call to myProduct
    squareOfProduct :: Int -> Int -> Int
    squareOfProduct x y = square (myProduct x y)

    squareOfProduct' :: Int -> Int -> Int
    squareOfProduct' x y = square ((\num1 num2 -> num1 * num2) x y)

    -- q5
    -- Rewrite squareOfProduct2 to use a lambda function instead of
    -- the function call to square
    squareOfProduct2 :: Int -> Int -> Int
    squareOfProduct2 x y = square (myProduct x y)

    squareOfProduct2' :: Int -> Int -> Int
    squareOfProduct2' x y = (\num1 -> num1 * num1) (myProduct x y)

    -- q6
    -- Rewrite the expression for v1 to use a lambda function
    -- which takes a list parameter and returns its first item
    lst1 = [3, 6, 2, 8]
    v1 = head lst1
    v1' = (\lst1 -> head lst1)

    --q7
    -- Write applyFuncToFirstElem so it takes two params, the first
    -- is a (Int -> Int) function and the second is a list of Ints, and
    -- returns the result of applying the function to the first item in
    -- the list.
    -- for example:
    -- incr x = x + 1
    -- applyFuncToFirstElem incr [10, 9, 8, 7]
    -- should return 11
    applyFuncToFirstElem :: (Int -> Int) -> [Int] -> Int
    applyFuncToFirstElem f1 lst1 = f1 (head lst1)

    --q8
    -- Rewrite the expression for v2 to use a lambda function instead of
    -- the call to someArithmetic
    someArithmetic :: Int -> Int
    someArithmetic x = 3 * x - 7

    v2 = applyFuncToFirstElem someArithmetic [10, 2, 3, 4]
    v2' = applyFuncToFirstElem (\x -> 3 * x - 7) [10, 2, 3, 4]

    -- q9
    -- map is a haskell function which takes two params, a function and a
    -- list. It applies the function to each item in the list, and returns
    -- a list of those results. See v3 and v4 for examples.
    -- Write an expression for v5 to first decrement and then multiply by
    -- two each element of lst1. Use decr and twice functions. (hint: dot operator)
    decr :: Int -> Int
    decr x = x - 1

    twice :: Int -> Int
    twice x = x * 2

    v3 = map decr lst1
    v4 = map twice lst1

    --v5 :: [Int] -> [Int]
    v5 = map (twice . decr) lst1

    -- q10
    -- The myMap function is supposed to be doing the same thing as the
    -- map function provided by Haskell. But myMap decr lst1 is giving the
    -- wrong answer. Find and fix the bug in myMap.
    myMap :: (Int -> Int) -> [Int] -> [Int]
    myMap f1 [] = []
    myMap f1 lst1 = (f1 (head lst1)) : myMap f1 (tail lst1)

    v6 = myMap decr lst1


