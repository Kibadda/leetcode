import Data.List (intercalate, transpose)

type Grid = [[Int]]

type Row = [Int]

type Col = [Int]

type Dia = [Int]

convert :: Int -> String
convert number
  | number > 0 = "8"
  | otherwise = "."

showLine :: [Int] -> String
showLine = unwords . map convert

showBoard :: Grid -> String
showBoard = intercalate "\n" . map showLine

printBoard :: Grid -> IO ()
printBoard = putStrLn . showBoard

----------------------------------------------

generateBoard :: Int -> Grid
generateBoard size = [[0 | y <- [0 .. (size - 1)]] | x <- [0 .. (size - 1)]]

rows :: Grid -> Grid
rows board = [x | x <- board]

cols :: Grid -> Grid
cols board = [x | x <- transpose board]

diagonals1 :: Grid -> Grid
diagonals1 board = [[y | (row, x) <- zip [0 ..] board, (col, y) <- zip [0 ..] x, row + col == k] | k <- [0 .. (2 * length board - 2)]]

diagonals2 :: Grid -> Grid
diagonals2 = diagonals1 . reverse . transpose

row :: Int -> Grid -> Row
row pos board = rows board !! (pos `div` length board)

col :: Int -> Grid -> Col
col pos board = cols board !! (pos `mod` length board)

dia :: Int -> Grid -> Dia
dia pos board =
  (diagonals1 board !! (posMod + posDiv)) ++ (diagonals2 board !! (length board - posMod + posDiv - 1))
  where
    posMod = pos `mod` length board
    posDiv = pos `div` length board

isValidInRow :: Row -> Bool
isValidInRow row = 1 `notElem` row

isValidInCol :: Col -> Bool
isValidInCol col = 1 `notElem` col

isValidInDia :: Dia -> Bool
isValidInDia dia = 1 `notElem` dia

isValidAtPos :: Int -> Grid -> Bool
isValidAtPos pos board =
  isValidInRow (row pos board) && isValidInCol (col pos board) && isValidInDia (dia pos board)

insertQueenAtPos :: Int -> Grid -> Grid
insertQueenAtPos pos board =
  rowsStart ++ (start ++ 1 : end) : rowsEnd
  where
    (rowsStart, row : rowsEnd) = splitAt (pos `div` length board) board
    (start, _ : end) = splitAt (pos `mod` length board) row

removeQueenAtPos :: Int -> Grid -> Grid
removeQueenAtPos pos board =
  rowsStart ++ (start ++ 0 : end) : rowsEnd
  where
    (rowsStart, row : rowsEnd) = splitAt (pos `div` length board) board
    (start, _ : end) = splitAt (pos `mod` length board) row

findLastQueen :: Int -> Grid -> Int
findLastQueen pos board
  | (board !! (pos `div` length board)) !! (pos `mod` length board) == 1 = pos
  | otherwise = findLastQueen (pos - 1) board

solve :: Int -> Grid -> Grid
solve pos board
  | pos < 0 = [[]]
  | posEnd && amountQueens == length board = board
  | posEnd && amountQueens < length board = solve (lastQueenPos + 1) (removeQueenAtPos lastQueenPos board)
  | isValidAtPos pos board = solve (pos + 1) (insertQueenAtPos pos board)
  | otherwise = solve (pos + 1) board
  where
    posEnd = pos == length board * length board
    lastQueenPos = findLastQueen (pos - 1) board
    amountQueens = sum (map sum board)

mainSolve :: Int -> IO ()
mainSolve size = printBoard (solve 0 (generateBoard size))

mainSolve' :: Int -> Int -> IO ()
mainSolve' size pos = printBoard (solve pos (generateBoard size))
