module BinaryTree
( BinaryTree(..)
, insert
, fromList
, contains
, search
, remove
) where

data BinaryTree k v = EmptyTree | BinaryNode { key :: k
                                             , val :: v
                                             , left :: BinaryTree k v
                                             , right :: BinaryTree k v
                                             } deriving (Eq, Show, Read)

--- Insert element with given key into the tree.
insert :: (Ord k) => k -> b -> BinaryTree k b -> BinaryTree k b
insert newKey newValue EmptyTree = BinaryNode newKey newValue EmptyTree EmptyTree
insert newKey newValue (BinaryNode key value left right)
        | newKey < key = BinaryNode key value (insert newKey newValue left) right
        | newKey > key = BinaryNode key value left (insert newKey newValue right)
        | newKey == key = BinaryNode newKey newValue left right

--- Function producing binary search tree from a list of tuples [(key, value)].
fromList :: (Ord k) => [(k, v)] -> BinaryTree k v
fromList = foldl (\acc curr -> insert (fst curr) (snd curr) acc) EmptyTree

--- Search for given key.
search :: (Ord k) => k -> BinaryTree k v -> Maybe v
search a EmptyTree = Nothing
search a (BinaryNode key value left right)
       | a < key = search a left
       | a > key = search a right
       | a == key = Just value

--- Checking if given key is in the tree.
contains :: (Ord k) => k -> BinaryTree k v -> Bool
contains a root = case search a root of
                       Nothing   -> False
                       Just das  -> True

--- Remove element with given key from the tree.
remove :: (Ord k) => k -> BinaryTree k v -> BinaryTree k v
remove toDel EmptyTree = EmptyTree
remove toDel node@(BinaryNode ckey value left right)
        | toDel == ckey = removeNode node
        | toDel < ckey = BinaryNode ckey value (remove toDel left) right
        | toDel > ckey = BinaryNode ckey value left (remove toDel right)
        where removeNode (BinaryNode ckey _ EmptyTree right) = right
              removeNode (BinaryNode ckey _ left EmptyTree) = left
              removeNode (BinaryNode ckey _ left right) = let s = mostLeft right
                                                          in BinaryNode (key s) (val s) (left) (remove (key s) right)
--- Get the most left (minimum) element in given branch.
mostLeft :: (Ord k) => BinaryTree k v -> BinaryTree k v
mostLeft node@(BinaryNode _ _ EmptyTree _) = node
mostLeft (BinaryNode _ _ left _) = mostLeft left
