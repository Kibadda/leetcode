input :: [Int]
input = [0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 9, 5, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 6, 0, 8, 0, 0, 0, 6, 0, 0, 0, 0, 4, 0, 0, 8, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 2, 8, 0, 0, 0, 0, 4, 1, 9, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 7, 0]

filled :: [Int]
filled = [x | x <- [0 .. 80], input !! x /= 0]

rows :: [Int]
rows = [x `div` 9 | x <- [0 .. 80]]

cols :: [Int]
cols = [x `mod` 9 | x <- [0 .. 80]]

blocks :: [Int]
blocks = [y `div` 3 + (x `div` 3) * 3 | x <- [0 .. 8], y <- [0 .. 8]]

row :: Int -> [Int] -> [Int]
row i xs = [x | (index, x) <- zip [0 .. 80] xs, rows !! i == rows !! index]

col :: Int -> [Int] -> [Int]
col i xs = [x | (index, x) <- zip [0 .. 80] xs, cols !! i == cols !! index]

block :: Int -> [Int] -> [Int]
block i xs = [x | (index, x) <- zip [0 .. 80] xs, blocks !! i == blocks !! index]

isValid :: Int -> Int -> [Int] -> Bool
isValid i x xs = (x `notElem` row i xs) && (x `notElem` col i xs) && (x `notElem` block i xs)

getNextFillable :: Int -> Int
getNextFillable i =
  if i + 1 `elem` filled
    then getNextFillable (i + 1)
    else i + 1

getLastFillable :: Int -> Int
getLastFillable i =
  if i - 1 `elem` filled
    then getLastFillable (i - 1)
    else i - 1

change :: Int -> Int -> [Int] -> [Int]
change i x xs =
  let (start, _ : end) = splitAt i xs
   in start ++ x : end

solve :: Int -> Int -> [Int] -> [Int]
solve 25 x xs = xs
solve i 10 xs = solve (getLastFillable i) ((xs !! getLastFillable i) + 1) (change i 0 xs)
solve i x xs =
  if isValid i x xs
    then solve (getNextFillable i) 1 (change i x xs)
    else solve i (x + 1) xs

startSolve :: [Int] -> [Int]
startSolve = solve 0 1

solve' :: Int -> Int -> ([Int], [(Int, Int, String)]) -> ([Int], [(Int, Int, String)])
solve' 24 x (xs, log) = (xs, log)
solve' i 10 (xs, log) = solve' (getLastFillable i) ((xs !! getLastFillable i) + 1) (change i 0 xs, (i, 10, "b") : log)
solve' i x (xs, log) =
  if isValid i x xs
    then solve' (getNextFillable i) 1 (change i x xs, (i, x, "i") : log)
    else solve' i (x + 1) (xs, (i, x, "u") : log)

startSolve' :: ([Int], [(Int, Int, String)]) -> ([Int], [(Int, Int, String)])
startSolve' = solve' 0 1

main :: IO ()
main = do
  -- print (startSolve' (input, [(0, 1, "a")]))
  print (startSolve input)
