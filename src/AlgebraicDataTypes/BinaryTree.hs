module AlgebraicDataTypes.BinaryTree where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)
  | otherwise = Leaf -- never happen

mapTree ::
  (a -> b) ->
  BinaryTree a ->
  BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = [x] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = inorder left ++ [x] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = postorder left ++ postorder right ++ [x]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f init = foldr f init . inorder

--
-- test cases

testTree' :: BinaryTree Integer
testTree' =
  Node
    (Node Leaf 3 Leaf)
    1
    (Node Leaf 4 Leaf)

mapExpected =
  Node
    (Node Leaf 4 Leaf)
    2
    (Node Leaf 5 Leaf)

testTree :: BinaryTree Integer
testTree =
  Node
    (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 (Node Leaf 4 Leaf))

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3, 4] then putStrLn "Preorder fine!" else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3, 4] then putStrLn "Inorder fine!" else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 4, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

-- acceptance test for mapTree
main :: IO ()
main = do
  if mapTree (+ 1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"
  testPreorder
  testInorder
  testPostorder
