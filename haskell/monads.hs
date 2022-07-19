data WithLog a = WithLog {value :: a, valueLog :: [String]} deriving (Show)

wrap :: a -> WithLog a
wrap x = WithLog {value = x, valueLog = []}

run :: WithLog a -> (a -> WithLog b) -> WithLog b
run WithLog {value = v, valueLog = l} fun =
  WithLog {value = newValue, valueLog = newLog ++ l}
  where
    newWithLog = fun v
    newValue = value newWithLog
    newLog = valueLog newWithLog

add :: Int -> Int -> WithLog Int
add x y = WithLog {value = newNumber, valueLog = ["added " ++ show x ++ " to " ++ show y ++ " to get " ++ show newNumber]}
  where
    newNumber = x + y
