-- Exercise Sheet 1, Assignment 1

myGcd :: Int -> Int -> Int
myGcd x y
  | x == y = x
  | y > x = myGcd y x
  | otherwise = myGcd (x-y) y

gcdInt :: Int -> Int -> Int
gcdInt x y =
  let (x', y') = (abs(x), abs(y))
  in myGcd x' y'