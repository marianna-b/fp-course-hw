{-# OPTIONS -Wall #-}
module HW2 where


{- 1 -}

safeTail :: [a] -> Either String [a]
safeTail [] = Left "Empty list in safeTail"
safeTail x = Right $ tail x

safeInit :: [a] -> Either String [a]
safeInit [] = Left "Empty list in safeInit"
safeInit x = Right $ init x

strip :: [a] -> Either String [a]
strip x = safeInit x >>= safeTail

{- 2 -}

type Health = Int
type Attack = Int
type Defence = Int

data Items = Power Attack | Medicine Health | Armor Defence
  deriving Show
type Equipment = [Items]

data Player = Player Health Attack Defence
  deriving Show
data Monster = Monster Health Attack Equipment
  deriving Show

type Result = Either Monster Player

class Fighter f where
  attack :: f -> Attack
  health :: f -> Health
  beAttacked :: f -> Int -> f

instance Fighter Player where
  attack (Player _ a _) = a
  health (Player h _ _) = h
  beAttacked (Player h a d) f = Player (h - damage) a d
    where damage = max 0 $ f - d * h `div` 100

instance Fighter Monster where
  attack (Monster _ d _) = d
  health (Monster h _ _) = h
  beAttacked (Monster h d e) f = Monster (h - f) d e

fight :: (Fighter a, Fighter b) => a -> b -> Either b a
fight f s
  | health f' > 0 && health s' > 0 = f' `fight` s'
  | health s' <= 0 = Right f
  | otherwise = Left s
  where (f', s') = (beAttacked f (attack s), beAttacked s (attack f))

applyEquipment :: Equipment -> Player -> Player
applyEquipment [] p = p
applyEquipment (Medicine m:xs) (Player h a d) = applyEquipment xs $ Player (h + m) a d
applyEquipment (Power nd:xs) (Player h a d) = applyEquipment xs $ Player h (a + nd) d
applyEquipment (Armor def:xs) (Player h a d) = applyEquipment xs $ Player h a $ d + def

gloriousBattle :: Player -> [Monster] -> Result
gloriousBattle p [] = Right p
gloriousBattle p (x@(Monster _ _ e):xs) = applyEquipment e `fmap` fight p x >>= (`gloriousBattle` xs)

{- 3 -}

data BinarySearchTree a = Leaf | Node a (BinarySearchTree a) (BinarySearchTree a)
  deriving Show

find :: Ord a => BinarySearchTree a -> a -> Maybe (BinarySearchTree a)
find Leaf _ = Nothing
find x@(Node val left right) key
  | val == key = Just x
  | val < key = find right key
  | otherwise = find left key

insert :: Ord a => BinarySearchTree a -> a -> BinarySearchTree a
insert Leaf key = Node key Leaf Leaf
insert x@(Node val left right) key
  | key < val = Node val (insert left key) right
  | key > val = Node val left (insert right key)
  | otherwise = x

getMin :: Ord a => BinarySearchTree a -> a
getMin Leaf = error "No minimum in empty tree"
getMin (Node val Leaf _) = val
getMin (Node _ left _) = getMin left

delete :: Ord a => BinarySearchTree a -> a -> BinarySearchTree a
delete Leaf _ = Leaf
delete (Node val left right) key
  | key < val = Node val (delete left key) right
  | key > val = Node val left (delete right key)
  | otherwise = case (left, right) of
                  (Leaf, Leaf) -> Leaf
                  (_, Leaf) -> left
                  (Leaf, _) -> right
                  _ -> Node newVal left $ delete right newVal
                    where newVal = getMin right

toList :: Ord a => BinarySearchTree a -> [a]
toList Leaf = []
toList (Node val left right) = val : combine (toList left) (toList right)
  where combine [] [] = []
        combine [] ys = ys
        combine xs [] = xs
        combine (x:xs) (y:ys) = x:y:combine xs ys

fromList :: Ord a => [a] -> BinarySearchTree a
fromList [] = Leaf
fromList (x:xs) = Node x (fromList l) $ fromList r
  where l = filter (x >) xs
        r = filter (> x) xs
