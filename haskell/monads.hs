data WithLog a = WithLog {getValue :: a, getLog :: [String]} deriving (Show)

instance Functor WithLog where
  fmap f (WithLog x log) = WithLog (f x) log

instance Applicative WithLog where
  pure f = WithLog f []
  (WithLog f log) <*> something = fmap f something

instance Monad WithLog where
  return x = WithLog x []
  WithLog x log >>= f = WithLog x' (log ++ log')
    where
      WithLog x' log' = f x

add :: Int -> Int -> WithLog Int
add x y = WithLog value log
  where
    value = x + y
    log = ["added " ++ show x ++ " to " ++ show y ++ " to get " ++ show value]

mul :: Int -> Int -> WithLog Int
mul x y = WithLog value log
  where
    value = x * y
    log = ["multiplied " ++ show x ++ " with " ++ show y ++ " to get " ++ show value]

sub :: Int -> Int -> WithLog Int
sub x y = WithLog value log
  where
    value = y - x
    log = ["subtracted " ++ show x ++ " from " ++ show y ++ " to get " ++ show value]

calculate :: WithLog Int
calculate = do
  start <- return 5 :: WithLog Int
  step1 <- add 10 start
  step2 <- mul 3 step1
  sub 9 step2
