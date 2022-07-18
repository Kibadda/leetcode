import Data.List (intercalate, transpose)

type Grid = [[Int]]

type Row = [Int]

type Col = [Int]

type Dia = [Int]

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
diagonals2 board = diagonals1 $ reverse $ transpose board

row :: Int -> Grid -> Row
row pos board = rows board !! (pos `div` length board)

col :: Int -> Grid -> Col
col pos board = cols board !! (pos `mod` length board)

dia :: Int -> Grid -> Dia
dia pos board = (diagonals1 board !! ((pos `mod` length board) + (pos `div` length board))) ++ (diagonals2 board !! (length board - (pos `mod` length board) + (pos `div` length board) - 1))

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
  let (rowsStart, row : rowsEnd) = splitAt (pos `div` length board) board
   in let (start, _ : end) = splitAt (pos `mod` length board) row
       in rowsStart ++ (start ++ 1 : end) : rowsEnd

removeQueenAtPos :: Int -> Grid -> Grid
removeQueenAtPos pos board =
  let (rowsStart, row : rowsEnd) = splitAt (pos `div` length board) board
   in let (start, _ : end) = splitAt (pos `mod` length board) row
       in rowsStart ++ (start ++ 0 : end) : rowsEnd

findLastQueen :: Int -> Grid -> Int
findLastQueen pos board
  | (board !! (pos `div` length board)) !! (pos `mod` length board) == 1 = pos
  | otherwise = findLastQueen (pos - 1) board

solve :: Int -> Grid -> Grid
solve pos board
  | pos < 0 = [[]]
  | pos == length board * length board =
    if sum (map sum board) == length board
      then board
      else solve (findLastQueen (pos - 1) board + 1) (removeQueenAtPos (findLastQueen (pos - 1) board) board)
  | isValidAtPos pos board = solve (pos + 1) (insertQueenAtPos pos board)
  | otherwise = solve (pos + 1) board

mainSolve :: Int -> IO ()
mainSolve size = printBoard (solve 0 (generateBoard size))

mainSolve' :: Int -> Int -> IO ()
mainSolve' size pos = printBoard (solve pos (generateBoard size))
