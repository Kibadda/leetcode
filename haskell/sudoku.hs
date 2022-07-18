import Data.List (intercalate, transpose)

type Grid = [[Int]]

type Row = [Int]

type Col = [Int]

type Box = [Int]

convert :: Int -> String
convert number
  | number > 0 = show number
  | otherwise = "."

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = take n l : chunksOf n (drop n l)
  | otherwise = error "Negative or zero"

showLine :: Row -> String
showLine =
  intercalate " | "
    . map unwords
    . chunksOf 3
    . map convert

showBoard :: Grid -> String
showBoard =
  intercalate "------+-------+------\n"
    . map unlines
    . chunksOf 3
    . map showLine

printBoard :: Grid -> IO ()
printBoard board = putStrLn $ showBoard board

-------------------------------------------------------------

-- input = [[9, 0, 0, 0, 2, 1, 3, 5, 0], [6, 0, 5, 0, 0, 7, 0, 0, 0], [0, 0, 0, 0, 0, 9, 8, 1, 0], [0, 9, 8, 1, 0, 3, 7, 0, 5], [0, 0, 0, 8, 0, 0, 0, 3, 4], [0, 4, 1, 0, 5, 0, 0, 0, 0], [1, 0, 0, 0, 7, 4, 5, 0, 9], [0, 5, 0, 0, 1, 0, 6, 2, 0], [8, 0, 9, 6, 0, 0, 0, 0, 0]]
input :: Grid
input = [[0, 3, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 1, 9, 5, 0, 0, 0], [0, 0, 8, 0, 0, 0, 0, 6, 0], [8, 0, 0, 0, 6, 0, 0, 0, 0], [4, 0, 0, 8, 0, 0, 0, 0, 1], [0, 0, 0, 0, 2, 0, 0, 0, 0], [0, 6, 0, 0, 0, 0, 2, 8, 0], [0, 0, 0, 4, 1, 9, 0, 0, 5], [0, 0, 0, 0, 0, 0, 0, 7, 0]]

filled :: [Int]
filled = [9 * row + col | row <- [0 .. 8], col <- [0 .. 8], (input !! row) !! col /= 0]

rows :: Grid -> Grid
rows board = [x | x <- board]

cols :: Grid -> Grid
cols board = [x | x <- transpose board]

boxes :: Grid -> Grid
boxes =
  map concat
    . chunksOf 3
    . concat
    . transpose
    . map (chunksOf 3)

row :: Int -> Grid -> Row
row pos board = rows board !! (pos `div` 9)

col :: Int -> Grid -> Col
col pos board = cols board !! (pos `mod` 9)

box :: Int -> Grid -> Box
box pos board
  | pos `elem` [0, 1, 2, 9, 10, 11, 18, 19, 20] = head (boxes board)
  | pos `elem` [3, 4, 5, 12, 13, 14, 21, 22, 23] = boxes board !! 3
  | pos `elem` [6, 7, 8, 15, 16, 17, 24, 25, 26] = boxes board !! 6
  | pos `elem` [27, 28, 29, 36, 37, 38, 45, 46, 47] = boxes board !! 1
  | pos `elem` [30, 31, 32, 39, 40, 41, 48, 49, 50] = boxes board !! 4
  | pos `elem` [33, 34, 35, 42, 43, 44, 51, 52, 53] = boxes board !! 7
  | pos `elem` [54, 55, 56, 63, 64, 65, 72, 73, 74] = boxes board !! 2
  | pos `elem` [57, 58, 59, 66, 67, 68, 75, 76, 77] = boxes board !! 5
  | otherwise = boxes board !! 8

isValidInRow :: Int -> Row -> Bool
isValidInRow number row = number `notElem` row

isValidInCol :: Int -> Col -> Bool
isValidInCol number col = number `notElem` col

isValidInBox :: Int -> Box -> Bool
isValidInBox number box = number `notElem` box

isValidAtPos :: Int -> Int -> Grid -> Bool
isValidAtPos number pos board =
  isValidInRow number (row pos board) && isValidInCol number (col pos board) && isValidInBox number (box pos board)

changeNumberAtPos :: Int -> Int -> Grid -> Grid
changeNumberAtPos number pos board =
  let (rowsStart, row : rowsEnd) = splitAt (pos `div` 9) board
   in let (start, _ : end) = splitAt (pos `mod` 9) row
       in rowsStart ++ (start ++ number : end) : rowsEnd

getNextFillablePos :: Int -> Int
getNextFillablePos pos
  | (pos + 1) `elem` filled = getNextFillablePos (pos + 1)
  | otherwise = pos + 1

getLastFillablePos :: Int -> Int
getLastFillablePos pos
  | (pos - 1) `elem` filled = getLastFillablePos (pos - 1)
  | otherwise = pos - 1

solve :: Int -> Int -> Grid -> Int -> Grid
solve number pos board cancelPos
  | pos < 0 = [[]]
  | pos > cancelPos = board
  | number > 9 = solve ((row pos board !! (getLastFillablePos pos `mod` 9)) + 1) (getLastFillablePos pos) (changeNumberAtPos 0 pos board) cancelPos
  | isValidAtPos number pos board = solve 1 (getNextFillablePos pos) (changeNumberAtPos number pos board) cancelPos
  | otherwise = solve (number + 1) pos board cancelPos

main :: IO ()
main = printBoard (solve 1 (getNextFillablePos (-1)) input 80)
