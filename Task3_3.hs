module Task3_3 where

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

-- класс Monoid определён в модуле Data.Monoid

-- class Monoid a where
--     mempty :: a
--     mappend :: a -> a -> a

-- В этом классе определено пустое значение mempty и бинарная функция соединения двух значений в одно.

-- свойства класса Monoid:
-- mempty `mappend` f = f
-- f `mappend` mempty = f
-- f `mappend` (g `mappend` h) = (f `mappend` g) `mappend` h

-- Для данной структуры данных можно реализовать: Объединение, Пересечение и Симметричную разность.
-- http://www.grandars.ru/student/vysshaya-matematika/mnozhestvo.html 

-- Объединение

newtype PSetUnion a = PSetUnion{ containsForUnion :: (a -> Bool) }

instance Monoid (PSetUnion a) where
    mempty  = PSetUnion (\_ -> False)
    mappend = (<>)

instance Semigroup (PSetUnion a) where
     (<>) (PSetUnion x) (PSetUnion y) = PSetUnion (\z -> x z || y z)

-- Пересечение

newtype PSetIntersection a = PSetIntersection{ containsForIntersection :: (a -> Bool) }

instance Monoid (PSetIntersection a) where
    mempty  = PSetIntersection (\_ -> True)
    mappend = (<>)

instance Semigroup (PSetIntersection a) where
     (<>) (PSetIntersection x) (PSetIntersection y) = PSetIntersection (\z -> x z && y z)

-- Симметричная разность

newtype PSetSymmetric a = PSetSymmetric{ containsForSymmetric :: (a -> Bool) }

instance Monoid (PSetSymmetric a) where
    mempty  = PSetSymmetric (\_ -> False)
    mappend = (<>)

instance Semigroup (PSetSymmetric a) where
     (<>) (PSetSymmetric x) (PSetSymmetric y) = PSetSymmetric (\z -> not ( x z && y z ) && ( x z || y z ))




