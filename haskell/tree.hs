data Tree = EmptyTree | Node Int Tree Tree deriving (Show)

invert :: Tree -> Tree
invert EmptyTree = EmptyTree
invert (Node x l r) = Node x (invert r) (invert l)
