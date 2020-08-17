import Test.QuickCheck
import Debug.Trace (trace)

data Tree a = Node a [Tree a]
  deriving (Show)

depth :: Tree a -> Int
depth (Node a []) = 1
depth (Node a l) = 1 + maximum (map depth l)

instance Eq a => Eq (Tree a) where
  (Node a1 l1) == (Node a2 l2) = (a1 == a2) && foldl (&&) True (zipWith (==) l1 l2)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    sized arbitrarySizedTree
  shrink (Node a l) = do
    l' <- shrink l
    return (Node a l')

arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree m = do
  t <- arbitrary
  n <- choose (0, m `div` 2)
  ts <- vectorOf n (arbitrarySizedTree (m `div` 4))
  return (Node t ts)

nodes :: Tree a -> Int
nodes (Node a []) = 1
nodes (Node a l) = 1 + sum (map nodes l)

root_id :: Tree (a, Int) -> Int
root_id (Node (_, id) _) = id

max_id :: Tree (a, Int) -> Int
max_id (Node (a, id) []) = id
max_id (Node (a, id) l) = max id $ maximum (map max_id l)

dfn :: Tree a -> Tree (a, Int)
dfn (Node a l) = let (tree, _) = dfn' 1 (Node a l)
                  in tree where
  dfn' id (Node a []) = (Node (a, id) [], id+1)
  dfn' id (Node a l) = let (tree, id') = iter_d (id+1) l
                        in (Node (a, id) tree, id') where
    iter_d id [] = ([], id)
    iter_d id ((Node a l) : xs) = let (tree, id') = dfn' id (Node a l)
                                      (treel, id'') = iter_d id' xs
                                   in ((tree : treel), id'')

bfn :: Tree a -> Tree (a, Int)
bfn t = t'
 where (t', ks') = aux ks t
       ks = 1 : ks'
       aux (k : ks) (Node x ts) = (Node (x, k) ts', (k+1) : ks')
         where (ts', ks') = auxs ks ts
       auxs ks [] = ([], ks)
       auxs ks (t : ts) = (t' : ts', ks'')
         where (t', ks') = aux ks t
               (ts', ks'') = auxs ks' ts

check_num_of_nodes :: (Tree a -> Tree b) -> Tree a -> Bool
check_num_of_nodes f (Node a l) = nodes (f (Node a l)) == nodes (Node a l)

check_root :: (Tree a -> Tree (a, Int)) -> Tree a -> Bool
check_root f (Node a l) = root_id (f (Node a l)) == 1

check_max_id :: (Tree a -> Tree (a, Int)) -> Tree a -> Bool
check_max_id f (Node a l) = max_id (f (Node a l)) == nodes (Node a l)

merge :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
merge f (Node a1 l1) (Node a2 l2) = Node (f a1 a2) (merge' f l1 l2) where
  merge' _ l1 [] = l1
  merge' _ [] l2 = l2
  merge' f (x1 : xs1) (x2 : xs2) = (merge f x1 x2) : merge' f xs1 xs2

wrong :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
wrong f (Node x tsx) (Node y tsy) = Node (f x y) $ zipWith (wrong f) tsx tsy

merges_are_equal :: Eq a => (a -> a -> a) -> Tree a -> Tree a -> Bool
merges_are_equal f t1 t2 = merge f t1 t2 == wrong f t1 t2

check_num_of_nodes_m :: (a -> a -> a) -> ((a -> a -> a) -> Tree a -> Tree a -> Tree a) -> Tree a -> Tree a -> Bool
check_num_of_nodes_m f m t1 t2 = max (depth t1) (depth t2) == depth (m f t1 t2)

main = do
  putStrLn "Checking number of nodes for dfn"
  quickCheck (check_num_of_nodes dfn :: Tree Int -> Bool)
  putStrLn "Checking number of nodes for bfn"
  quickCheck (check_num_of_nodes bfn :: Tree Int -> Bool)
  putStrLn "Checking root = 1 for dfn"
  quickCheck (check_root dfn :: Tree Int -> Bool)
  putStrLn "Checking root = 1 for bfn"
  quickCheck (check_root bfn :: Tree Int -> Bool)
  putStrLn "Checking maximum id for dfn"
  quickCheck (check_max_id dfn :: Tree Int -> Bool)
  putStrLn "Checking maximum id for bfn"
  quickCheck (check_max_id bfn :: Tree Int -> Bool)
  putStrLn "Checking merge == wrong"
  quickCheck (merges_are_equal (+) :: Tree Int -> Tree Int -> Bool)
  putStrLn "Checking depth of wrong result"
  quickCheck (check_num_of_nodes_m (+) wrong :: Tree Int -> Tree Int -> Bool)
  putStrLn "Checking depth of merge result"
  quickCheck (check_num_of_nodes_m (+) merge :: Tree Int -> Tree Int -> Bool)
