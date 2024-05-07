import Data.Char

-- name: Daniel Canhedo
-- SUID: 592676510

-- placeholder function so file compiles
-- like 'pass' in Python labs

yourCodeGoesHere = error "todo"


-- q1
-- Write a recursive implementation of uncompress, which is the reverse of
-- compress from HW 9. So compressing any string and then uncompressing it
-- should give the original string.
-- For example, if s1 = "a2b2c2" return "aabbcc".
-- If s1 = "sb3c2" return "abbbcc".
-- If s1 = "ab5" return "abbbbb"

uncompress :: String -> String
uncompress [] = ""
uncompress [x] = [x]
uncompress (x:y:ys)
            | isDigit y && digitToInt y > 1 = x : uncompress (x : show (digitToInt y - 1) ++ ys)
            | isDigit y = x : uncompress ys
            | otherwise = x : uncompress (y:ys)

                        

-- q2
-- Write a recursive implementation for firstWord.
-- Assume that s1 will have only chars a-z and spaces, and may have many
-- consecutive spaces anywhere in the String.
-- Words are series of chars a-z separated by one or more spaces.
-- For example, s1 = "one two three" return "one"
-- For example, s1 = "  one   two   three   " return "one"
-- You may use lstrip which removes all leading spaces from a String.

lstrip :: String -> String
lstrip "" = ""
lstrip (x:xs) = if x == ' '
                then lstrip xs
                else x : xs

firstWord :: String -> String
firstWord [] = ""
firstWord val
        | head val == ' ' = firstWord (tail val)
        | otherwise = head val : if tail val == "" || head (tail val) == ' ' then "" else firstWord (tail val)



-- q3
-- Write a recursive implementation of splitOnSpace.
-- Assume that s1 will have only chars a-z and spaces, and may have many
-- consecutive spaces anywhere in the String.
-- Words are series of chars a-z separated by one or more spaces.
-- For example, s1 = "one two three" return ["one", "two", "three"]
-- For example, s1 = "  one   two  three   " return ["one", "two", "three"]

splitOnSpace :: String -> [String]
splitOnSpace [] = []
splitOnSpace val
  | head val == ' ' = splitOnSpace (dropWhile (== ' ') val)
  | otherwise = 
      let word = takeWhile (/= ' ') val
          rest = dropWhile (== ' ') (drop (length word) val)
      in word : (if rest == "" then [] else splitOnSpace rest)


-- q4
-- The following code defines two type synonyms (FirstName and LastName),
-- and a new data type (Name). Modify the definition of Name to use the type
-- synonyms.
type FirstName = String
type LastName = String

data Name = Name FirstName LastName

-- q5
-- Write the implementation of (==) for Name, so that two names can be
-- compared. The comparison should be case insensitive. So if
-- n1 = Name "John" "Doe"
-- n2 = Name "john" "doe"
-- n1 == n2 should be True.
-- Hint: use pattern matching to access data in Name.

instance Eq Name where
  (==) (Name f1 l1) (Name f2 l2) =
                  (map toLower f1 == map toLower f2) && (map toLower l1 == map toLower l2)


-- q6
-- Write an implementation for printName which takes a Name object and
-- returns the string: "lastName, firstName"
-- Hint: use pattern matching to access data in Name.
printName :: Name -> String
printName (Name f1 l1) = l1 ++ ", " ++ f1


-- q7
-- Write an implementation for show which does the same thing as printName from q6.

instance Show Name where
  show (Name f1 l1) = l1 ++ ", " ++ f1


-- q8
-- Given the following definition of Point, write an implementation of
-- printPoint which returns a string: "(x, y)"
type XCoord = Int
type YCoord = Int

data Point = Point {xcoord::XCoord, ycoord::YCoord}
-- how to create a Point object
examplePoint = Point {xcoord=2, ycoord=3}
-- how to get data from a Point object
extractX = xcoord examplePoint

printPoint :: Point -> String
printPoint p1 = "(" ++ show (xcoord p1) ++ ", " ++ show (ycoord p1) ++ ")"

-- q9
-- Write an implementation for (==) so two Point objects can be compared.

instance Eq Point where
  (==) p1 p2 = (xcoord p1 == xcoord p2) && (ycoord p1 == ycoord p2)


-- q10
-- Write an implementation for show so Point objects can be printed to
-- match the output of printPoint.

instance Show Point where
  show p1 = "(" ++ show (xcoord p1) ++ ", " ++ show (ycoord p1) ++ ")"
