import Data.List (intercalate, transpose)

type Grid = [[Int]]

type Row = [Int]

type Col = [Int]

type Street = [Int]

convert :: Int -> String
convert number
  | number < 0 = '*' : show (number * (-1))
  | number >= 10 = "**"
  | otherwise = ' ' : show number

showLine :: Row -> String
showLine = unwords . map convert

showBoard :: Grid -> String
showBoard = intercalate "\n" . map showLine

printBoard :: Grid -> IO ()
printBoard board = putStrLn $ showBoard board

----------------------------------------------------

input :: Grid
input = [[10, 0, 3, 0, 10, 0, 0, 7, 10], [-1, 0, 0, 0, 10, 0, 7, 0, 9], [0, 0, -2, 0, 0, 10, 10, 0, 0], [2, 0, 10, 0, 6, 0, 0, 0, 7], [10, 10, 5, 0, 10, 3, 0, 10, -6], [0, 0, 0, 0, 0, 0, 10, 0, 0], [0, 0, 10, -9, 4, 0, 10, 0, 3], [0, 0, 8, 7, 10, 0, 0, 0, 10], [10, 9, 0, 0, -3, 0, 5, 4, 10]]

filled :: [Int]
filled = [9 * row + col | row <- [0 .. 8], col <- [0 .. 8], (input !! row) !! col /= 0]

rows :: Grid -> Grid
rows board = [x | x <- board]

cols :: Grid -> Grid
cols board = [x | x <- transpose board]

row :: Int -> Grid -> Row
row pos board = rows board !! (pos `div` 9)

col :: Int -> Grid -> Col
col pos board = cols board !! (pos `mod` 9)

getStreetInRow :: Int -> Grid -> Int -> Street
getStreetInRow pos board initialPos
  | number < 0 || number > 9 || pos `div` 9 /= initialPos `div` 9 = []
  | pos == initialPos = getStreetInRow (pos - 1) board initialPos ++ getStreetInRow (pos + 1) board initialPos
  | pos < initialPos = getStreetInRow (pos - 1) board initialPos ++ [number]
  | otherwise = number : getStreetInRow (pos + 1) board initialPos
  where
    number = (board !! (pos `div` 9)) !! (pos `mod` 9)

getStreetInCol :: Int -> Grid -> Int -> Street
getStreetInCol pos board initialPos
  | pos < 0 || pos > 80 || number < 0 || number > 9 = []
  | pos == initialPos = getStreetInCol (pos - 9) board initialPos ++ getStreetInCol (pos + 9) board initialPos
  | pos < initialPos = getStreetInCol (pos - 9) board initialPos ++ [number]
  | otherwise = number : getStreetInCol (pos + 9) board initialPos
  where
    number = (board !! (pos `div` 9)) !! (pos `mod` 9)

isValidInRow :: Int -> Row -> Bool
isValidInRow number row = number `notElem` row

isValidInCol :: Int -> Col -> Bool
isValidInCol number col = number `notElem` col

checkStreet :: Int -> Street -> Bool
checkStreet size street = maximum street - minimum street <= size - 1

removeZeros :: Street -> Street
removeZeros street = [x | x <- street, x /= 0]

isValidInStreet :: Int -> Int -> Grid -> Bool
isValidInStreet number pos board =
  checkStreet (length streetRow + 1) (number : removeZeros streetRow) && checkStreet (length streetCol + 1) (number : removeZeros streetCol)
  where
    streetRow = getStreetInRow pos board pos
    streetCol = getStreetInCol pos board pos

isValidAtPos :: Int -> Int -> Grid -> Bool
isValidAtPos number pos board =
  isValidInRow number (map abs (row pos board)) && isValidInCol number (map abs (col pos board))

changeNumberAtPos :: Int -> Int -> Grid -> Grid
changeNumberAtPos number pos board =
  rowsStart ++ (start ++ number : end) : rowsEnd
  where
    (rowsStart, row : rowsEnd) = splitAt (pos `div` 9) board
    (start, _ : end) = splitAt (pos `mod` 9) row

getNextFillablePos :: Int -> Int
getNextFillablePos pos
  | (pos + 1) `elem` filled = getNextFillablePos (pos + 1)
  | otherwise = pos + 1

getLastFillablePos :: Int -> Int
getLastFillablePos pos
  | (pos - 1) `elem` filled = getLastFillablePos (pos - 1)
  | otherwise = pos - 1

solve :: Int -> Int -> Grid -> Grid
solve number pos board
  | pos < 0 = [[]]
  | pos >= length board * length board = board
  | number > 9 = solve (numberAtLastPos + 1) lastPos (changeNumberAtPos 0 pos board)
  | valid = solve 1 nextPos (changeNumberAtPos number pos board)
  | otherwise = solve (number + 1) pos board
  where
    lastPos = getLastFillablePos pos
    nextPos = getNextFillablePos pos
    numberAtLastPos = row pos board !! (lastPos `mod` 9)
    valid = isValidAtPos number pos board && isValidInStreet number pos board

solveWithLog :: Int -> Int -> (Grid, [(Int, Int, Char)]) -> (Grid, [(Int, Int, Char)])
solveWithLog number pos (board, log)
  | pos < 0 = ([[]], log)
  | pos >= length board * length board = (board, log)
  | number > 9 = solveWithLog ((row pos board !! (lastPos `mod` 9)) + 1) lastPos (changeNumberAtPos 0 pos board, (pos, number, 'b') : log)
  | valid = solveWithLog 1 nextPos (changeNumberAtPos number pos board, (pos, number, 'i') : log)
  | otherwise = solveWithLog (number + 1) pos (board, log)
  where
    lastPos = getLastFillablePos pos
    nextPos = getNextFillablePos pos
    valid = isValidAtPos number pos board && isValidInStreet number pos board

main :: IO ()
main = printBoard (solve 1 (getNextFillablePos (-1)) input)
