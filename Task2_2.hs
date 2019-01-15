module Task2_2 where

import Todo(todo)

import Data.List (splitAt)
import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f r []     = r
foldl f r (x:xs) = foldl f (f r x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f r []     = r
foldr f r (x:xs) = f x (foldr f r xs)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f r = case f r of 
                Just (x, r) -> x : (unfoldr f r)
                Nothing     -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f lst = foldr (\h t -> f h : t) [] lst

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldr (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes lst = foldr (\h t -> case h of 
                        Just j  -> j : t
                        Nothing -> t) [] lst

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal lst = snd (foldr (\h (i, t) -> (i - 1, h !! i : t)) (length lst - 1, []) lst)

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f lst = foldl (\h t -> if not . f $ t then t : h else h) [] lst

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem el lst = foldl (\h t -> h || el == t) False lst

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr (\h -> if h < to then Just (h, h + step) else Nothing) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append lst1 lst2 = foldl (\h t -> t : h) lst2 (reverse lst1)

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr (\h -> if not . null $ h then Just (splitAt (fromIntegral n) h) else Nothing) lst
