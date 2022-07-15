import Data.List (intercalate, transpose)

type Grid = [[Int]]

type Row = [Int]

type Col = [Int]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = take n l : chunksOf n (drop n l)
  | otherwise = error "Negative or zero"

showLine :: [Int] -> String
showLine = unwords . map show

showBoard :: Grid -> String
showBoard = intercalate "\n" . map showLine

printBoard :: Grid -> IO ()
printBoard board = putStrLn $ showBoard board

input :: Grid
input = [[0 | y <- [0 .. 7]] | x <- [0 .. 7]]

rows :: Grid -> Grid
rows board = [x | x <- board]

cols :: Grid -> Grid
cols board = [x | x <- transpose board]

row :: Int -> Grid -> Row
row pos board = rows board !! (pos `div` length board)

col :: Int -> Grid -> Col
col pos board = cols board !! (pos `mod` length board)

isValidInRow :: Row -> Bool
isValidInRow row = 1 `notElem` row

isValidInCol :: Col -> Bool
isValidInCol col = 1 `notElem` col

isValidAtPos :: Int -> Grid -> Bool
isValidAtPos pos board =
  isValidInRow (row pos board) && isValidInCol (col pos board)

insertQueenAtPos :: Int -> Grid -> Grid
insertQueenAtPos pos board =
  let (rowsStart, row : rowsEnd) = splitAt (pos `div` length board) board
   in let (start, _ : end) = splitAt (pos `mod` length board) row
       in rowsStart ++ (start ++ 1 : end) : rowsEnd

solve :: Int -> Grid -> Grid
solve pos board
  | pos < 0 = [[]]
  | pos >= length board ^ 2 = board
  | isValidAtPos pos board = solve (pos + 1) (insertQueenAtPos pos board)
  | otherwise = solve (pos + 1) board

main :: IO ()
main = printBoard (solve 0 input)
