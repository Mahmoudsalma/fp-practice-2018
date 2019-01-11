module Task2_1 where

import Todo(todo)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTree 
                | Node {left::(TreeMap v), right::(TreeMap v), key::Integer, value::v} deriving Show

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains (Node left right key value) searchKey
        | key == searchKey = True
        | key <  searchKey = contains left searchKey
        | key >  searchKey = contains right searchKey
contains _ _ = False 

-- Значение для заданного ключа
lookup' :: Integer -> TreeMap v -> v
lookup' searchKey EmptyTree = error "the key is not found"
lookup' searchKey (Node left right key value) 
        | searchKey == key = value
        | searchKey <  key = lookup' searchKey left
        | searchKey >  key = lookup' searchKey right

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (key, value) EmptyTree = Node EmptyTree EmptyTree key value
insert (key, value) (Node left right nodeKey nodeValue) 
        | key == nodeKey = Node left right nodeKey value
        | key <  nodeKey = Node (insert (key, value) left) right nodeKey nodeValue
        | key >  nodeKey = Node left (insert (key, value) right) nodeKey nodeValue

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove _ EmptyTree = error "deleted key not found"
remove searchKey (Node left right key value) 
        | searchKey >  key = Node left (remove searchKey right) key value
        | searchKey <  key = Node (remove searchKey left) right key value
        | searchKey == key = removeKey searchKey (Node left right key value)

removeKey :: Integer -> TreeMap v -> TreeMap v
removeKey searchKey (Node EmptyTree EmptyTree _ _ ) = EmptyTree
removeKey searchKey (Node EmptyTree right     _ _ ) = right
removeKey searchKey (Node left      EmptyTree _ _ ) = left
removeKey _         (Node left      right     _ _ ) = removeKey' left right

removeKey' :: TreeMap v -> TreeMap v -> TreeMap v
removeKey' left right = Node left (remove key right) key value
                        where (key, value) = removeKey'' right

removeKey'' :: TreeMap v -> (Integer, v)
removeKey'' (Node EmptyTree right key value) = (key, value)
removeKey'' (Node left      right key value) = removeKey'' left

-- Поиск ближайшего снизу ключа относительно заданного
-- nearestLE :: Integer -> TreeMap v -> (Integer, v)
-- nearestLE _ EmptyTree = error "search key not found"

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList []     = EmptyTree
treeFromList (x:xs) = insert x (treeFromList xs)

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = todo

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = todo
