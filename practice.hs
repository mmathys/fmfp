-- pascal exercise

pascal :: Int -> [Int]
pascal 0 = [1]
pascal n = zipWith (+) ((pascal (n - 1))++[0]) ([0]++(pascal (n - 1)))

-- take

take2 :: Int -> [a] -> [a]
take2 0 _ = []
take2 n (x:xs) = x:(take2 (n-1) xs)

-- drop
drop2 :: Int -> [a] -> [a]
drop2 0 x = x
drop2 n (x:xs) = drop2 (n-1) xs

-- list
f :: (RealFloat x) => x -> [Int]
f n = [1..s] where s = (round . sqrt) n