import Prelude hiding (Word)

type State = Int
type Alphabet a = [a]
type DFA a = 
  ( Alphabet a             -- alphabet
  , State                  -- initial state
  , State -> a -> State    -- transition function
  , State -> Bool)         -- test for final state
type Word a = [a]

alphabet :: DFA a -> Alphabet a
alphabet (a, _, _, _) = a

initial :: DFA a -> State
initial (_, i, _, _) = i

transition :: DFA a -> (State -> a -> State)
transition (_, _, t, _) = t

finalState :: DFA a -> State -> Bool
finalState (_, _, _, f) = f

{-
   Please shortly indicate why using accessor functions is useful.
   
   accessor functions are a shorthand for pattern matching. it simplifies code
-}

accepts :: DFA a -> Word a -> Bool
-- solved using explicit recursion
{-
accepts dfa word = finalState dfa state where
  state = aux (initial dfa) word where
    aux state [] = state
    aux state (x:w) = aux ((transition dfa) state x) w
-}

--solved using foldl
accepts dfa word = finalState dfa final where
  final = foldl (transition dfa) (initial dfa) word

lexicon :: Alphabet a -> Int -> [Word a]
lexicon alphabet 0 = [[]]
lexicon alphabet n = foldr (++) [] $ map (\el -> map (\a -> el ++ [a]) alphabet) prev where
  prev = lexicon alphabet (n - 1)

language :: DFA a -> Int -> [Word a]
language dfa n = filter (\w -> accepts dfa w) $ lexicon (alphabet dfa) n

-- Try to use map, foldl, foldr, filter and/or list comprehensions.

-- assignment 3

-- map with aux
map' :: (a -> a) -> [a] -> [a]
map' f [] = []
map' f (x:xs) = (f x) : map' f xs

-- concat with foldr
concat' :: [[a]] -> [a]
concat' list = foldr (++) [] list

-- combined
concatMap' f = foldr aux e where
  aux a b = f a ++ b
  e = []

myFoldl f v l = undefined

-- headache of the week

isHarshad :: Int -> Bool
isHarshad n = n `mod` quersumme n == 0

quersumme :: Int -> Int
quersumme 0 = 0
quersumme x = x `mod` 10 + quersumme (x `div` 10)

-- assignment 4

{-
   In this exercise you are required to adapt the following function implementations of
   f, g and h such that foldl, foldr, zip, zipWith, filter, curry, uncurry, etc. will
   be used. That means, your task is to modify the lines 10-11, 15-19 and 23-28.
-}

f :: [[a]] -> [a]
f = (foldr (++) []) . (map (\x -> reverse x)) 


g :: Eq a => [a] -> [a] -> [a]
g xs ys = map (\(x, y) -> x) (filter (\(x, y) -> x == y) (zip xs ys))


h :: [Int] -> Int
h = length . filter (\x -> even x)