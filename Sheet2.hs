-- square root

improve :: Double -> Double -> Double
improve x y = (y + (x/y)) / 2.0

goodEnough :: Double -> Double -> Bool
goodEnough y y' = abs ((y-y') / y) < eps
    where eps = 0.001

root :: Double -> Double
root x = aux 1 where
    aux :: Double -> Double
    aux y
        | goodEnough y y' = y'
        | otherwise = aux y'
        where y' = improve x y

main :: IO ()
main = do
    putStrLn "Compute the root of:"
    input <- getLine
    let x = read input
    let r = root x
    putStrLn("root: " ++ show r)

-- assignment 6

-- first map, then filter
filterMap :: (b -> Bool) -> (a -> b) -> [a] -> [b]
filterMap p f = filter p . map f

-- map with aux
auxMap :: (Int -> Int) -> [Int] -> [Int]
auxMap f = foldr aux [] where
    aux = ((:) . f) 

-- filter with aux
auxFilter :: (Int -> Bool) -> [Int] -> [Int]
auxFilter f = foldr aux [] where
    aux a b = if f a then a : b else b

-- combine approaches
filterMap' :: (b -> Bool) -> (a -> b) -> [a] -> [b]
filterMap' p f = foldr aux [] where
    aux = (filter' . f) where
        filter' a b = if p a then a : b else b