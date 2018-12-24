module Task1_2 where

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo


-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd' :: Integer -> Integer -> Integer
gcd' x 0 = x
gcd' x y = gcd' y (x `rem` y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = ceiling(sqrt . fromIntegral $ from) < ceiling (sqrt . fromIntegral $ to)


-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = if day >= 1 
                               then case month of
                               1  -> day <= 31
                               2  -> if isLeapYear year then day <= 29 else day <= 28
                               3  -> day <= 31
                               4  -> day <= 30
                               5  -> day <= 31
                               6  -> day <= 30
                               7  -> day <= 31
                               8  -> day <= 31
                               9  -> day <= 30
                               10 -> day <= 31
                               11 -> day <= 30
                               12 -> day <= 31
                               else False
isLeapYear :: Integer -> Bool
isLeapYear year = year `mod` 400 == 0 || (year `mod` 100 /= 0 && year `mod` 4 == 0)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow' :: Integer -> Integer -> Integer
pow' x 0 = 1
pow' x 1 = x
pow' x y = x * pow' x (y - 1)

pow'' :: Integer -> Integer -> Integer
pow'' x y | y == 0    = 1
          | y == 1    = x
          | odd y     = x * pow'' x (y - 1)
          | otherwise = (pow'' x (y `div` 2)) * (pow'' x (y `div` 2))

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = [y | y <- [1..x], x `mod` y == 0] == [1,x]

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo