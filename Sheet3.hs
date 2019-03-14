-- Assignment 2

xor :: Bool -> Bool -> Bool
xor x y = (x && (not y)) || ((not x) && y)

-- with map and zip
otp2 :: [Bool] -> [Bool] -> [Bool]
otp2 message key = map (uncurry xor) (zip message key)

-- with zipWith
otp :: [Bool] -> [Bool] -> [Bool]
otp message key = zipWith xor message key

-- Assignment 3
prime :: Int -> Bool
prime n = [x | x <- [1..n], n `mod` x == 0] == [1,n]

primes :: Int -> [Int]
primes m = [x | x <- [1..m], prime x]

firstPrimes :: Int -> [Int]
firstPrimes m = numPrimesFrom 1 m where
    numPrimesFrom :: Int -> Int -> [Int]
    numPrimesFrom from 0 = []
    numPrimesFrom from num =
        let isPrime = (prime from) in
            if isPrime
                then from : numPrimesFrom (from + 1) (num - 1)
                else numPrimesFrom (from + 1) num

-- palindromes
palindromes :: [String] -> [String]
palindromes list = [l ++ r | l <- list, r <- list, l ++ r == reverse (l ++ r)]

-- words
split :: Char -> String -> [String]
split sep s = aux s "" where
    aux [] w = [w]
    aux (c:cs) w
        | c == sep = w : aux cs "" 
        | otherwise = aux cs (w++[c])

-- quick exercise
countAs :: String -> Int
countAs s = aux s 0 where
    aux [] sum = sum
    aux (c:cs) sum = if c == 'a' || c == 'A' then aux cs (sum + 1) else aux cs sum

isASpace :: Char -> Bool
isASpace c = c == ' '

toWords :: String -> [String]
toWords s = filter (\x -> x /= "") (split ' ' s)

countWords :: String -> Int
countWords s = length (toWords s)