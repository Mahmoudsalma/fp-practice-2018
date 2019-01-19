module Task3_2 where

import Todo(todo)

myR1 = RCons (RNil) 1
myR2 = RCons (myR1) 2
myR3 = RCons (myR2) 3

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList z = foldl (\x y -> y:x) [] z

listToRList :: [a] -> ReverseList a
listToRList z = foldl (\x y -> RCons x y) RNil z

instance Foldable ReverseList where
  foldr _ z RNil                  = z
  foldr f z (RCons reverseList x) = f x (foldr f z reverseList)

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Eq a) => Eq (ReverseList a) where
    (==) RNil          RNil          = True 
    (==) (RCons x1 y1) (RCons x2 y2) = x1 == x2 && y1 == y2
    (==) _             _             = False

instance (Ord a) => Ord (ReverseList a) where
    (<=) x y = (<=) x' y'
        where x' = rlistToList x
              y' = rlistToList y

instance (Show a) => Show (ReverseList a) where
    show RNil        = "[]"
    show (RCons x y) = show x ++ ", " ++ show y

instance Monoid (ReverseList a) where
    mempty  = RNil
    mappend = (<>)

instance Semigroup (ReverseList a) where
  (<>) x y = foldr (\h t -> RCons t h) y x

instance Functor ReverseList where
    fmap _ RNil        = RNil
    fmap f (RCons x y) = RCons (fmap f x) (f y)  
