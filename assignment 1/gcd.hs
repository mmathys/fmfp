-- Exercise Sheet 1, Assignment 1

myGcd :: Int -> Int -> Int
myGcd x y
  | x == y = x
  | y > x = myGcd y x
  | otherwise = myGcd (x-y) y
