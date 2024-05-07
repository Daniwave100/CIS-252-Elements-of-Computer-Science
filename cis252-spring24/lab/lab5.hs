-- Daniel Canhedo
-- docanhed@syr.edu

listOf :: Integer -> Float -> [Float] 
listOf n item 
    | n == 0 = [] 
    | n > 0 = item : listOf (n-1) item 
    | otherwise = error "listOf: requires nonnegative input"

rotate3 :: Integer -> Char -> Char -> Char -> String
rotate3 n item1 item2 item3
    | n == 0 = ""
    | n > 0 = item1 : rotate3 (n-1) item2 item3 item1
    | otherwise = error "rotate3: requires nonnegative input"

descend :: Integer -> Char -> Char -> [String]
descend n item1 item2
    | n == 0 = []
    | n > 0 = (rotate3 n item1 item1 item1) : descend (n-1) item2 item1