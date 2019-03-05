-- Formulas exercise

mult :: Int -> Int -> Int
mult x y
  | x == 0 = 0
  | x < 0 && y >= 0 = mult y x
  | x < 0 && y < 0 = mult (-x) (-y)
  | otherwise = y + mult (x - 1) y

log2 :: Int -> Int
log2 x = fst (aux x)

    