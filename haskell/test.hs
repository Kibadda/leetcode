qsort [] = []
qsort (x : xs) = qsort small ++ mid ++ qsort large
  where
    small = [y | y <- xs, y < x]
    mid = x : [y | y <- xs, y == x]
    large = [y | y <- xs, y > x]

isAnagram :: String -> String -> Bool
isAnagram a b = qsort a == qsort b
