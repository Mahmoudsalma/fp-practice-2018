module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа


-- Для тестирования функций
my4  = Succ . Succ . Succ . Succ $ Zero
my3  = Succ . Succ . Succ $ Zero
my2  = Succ . Succ $ Zero
my1  = Succ $ Zero
mym1 = Pred $ Zero
mym2 = Pred . Pred $ Zero
mym3 = Pred . Pred . Pred $ Zero
mym4 = Succ . Pred . Pred . Pred . Pred . Pred $ Zero
myz  = Zero

instance Eq WeirdPeanoNumber where
    (==) Zero Zero         = True
    (==) (Succ x) (Succ y) = x == y
    (==) (Pred x) (Pred y) = x == y
    (==) _        _        = False

instance Show WeirdPeanoNumber where 
    show Zero     = "Zero"
    show (Succ x) = "Succ " ++ show x
    show (Pred x) = "Pred " ++ show x

instance Real WeirdPeanoNumber where
    toRational x = toRational . toInteger $ x

instance Enum WeirdPeanoNumber where
    toEnum 0 = Zero
    toEnum x |x > 0 = Succ(toEnum(x - 1))
             |x < 0 = Pred(toEnum(x + 1))
    fromEnum Zero     = 0
    fromEnum (Succ x) = fromEnum x + 1
    fromEnum (Pred x) = fromEnum x - 1

instance Ord WeirdPeanoNumber where
     (<=) x y = lessThanOrEqualTo (relieve x) (relieve y) 

lessThanOrEqualTo :: WeirdPeanoNumber -> WeirdPeanoNumber -> Bool
lessThanOrEqualTo (Succ x) (Succ y) = x <= y
lessThanOrEqualTo (Pred x) (Pred y) = x <= y
lessThanOrEqualTo (Pred _) Zero     = True
lessThanOrEqualTo Zero     (Succ _) = True
lessThanOrEqualTo x        y        = x == y

instance Integral WeirdPeanoNumber where
    toInteger Zero     = toInteger 0
    toInteger (Succ x) = toInteger x + 1
    toInteger (Pred x) = toInteger x - 1

    quotRem x y = case (signum x, signum y) of
        (_        , Zero     ) -> error "division by zero is prohibited"
        (Zero     , _        ) -> (Zero, Zero)
        (Succ Zero, Succ Zero) -> divide x y Zero
        (Succ Zero, Pred Zero) -> negateX  (divide x    (-y) Zero)
        (Pred Zero, Succ Zero) -> negateXY (divide (-x) y    Zero)
        (Pred Zero, Pred Zero) -> negateY  (divide (-x) (-y) Zero)
        where negateX  (q, r) = (-q, r)
              negateY  (q, r) = (q, -r)
              negateXY (q, r) = (-q, -r)

divide :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber -> (WeirdPeanoNumber, WeirdPeanoNumber)
divide x y z | x >= y = divide (relieve (x-y)) y (Succ z)
             | otherwise = (z, x)

relieve :: WeirdPeanoNumber -> WeirdPeanoNumber
relieve (Succ (Pred x)) = relieve x
relieve (Pred (Succ x)) = relieve x
relieve (Succ x)        = Succ (relieve x)
relieve (Pred x)        = Pred (relieve x)
relieve _               = Zero

instance Num WeirdPeanoNumber where
    (+) x y = plusNum (relieve x) (relieve y)
    (*) x y = multiplicationNum (relieve x) (relieve y)

    negate Zero     = Zero
    negate (Pred x) = Succ (negate x)
    negate (Succ x) = Pred (negate x)

    abs x | signum x == 0    = Zero
          | signum x == (-1) = negate x
          | otherwise        = x

    signum x | x == Zero = 0
             | x <  Zero = (-1)
             | otherwise = 1

    fromInteger x | (x == 0)  = Zero
                  | (x >  0)  = Succ $ fromInteger (x - 1)
                  | otherwise = Pred $ fromInteger (x + 1)

plusNum x1 y1 = case (x1, y1) of
    (Pred x, Pred y) -> x + Pred (Pred y)
    (Succ x, Succ y) -> x + Succ (Succ y)
    (Pred x, Succ y) -> x + y
    (Succ x, Pred y) -> x + y
    (x     , Zero  ) -> x
    (Zero  , y     ) -> y

multiplicationNum x y = case (signum x, signum y) of
    (_        , Zero     ) -> Zero
    (Zero     , _        ) -> Zero
    (Succ Zero, Succ Zero) -> multiplication x y
    (Succ Zero, Pred Zero) -> -(multiplication x (-y))
    (Pred Zero, Succ Zero) -> -(multiplication (-x) y)
    (Pred Zero, Pred Zero) -> multiplication (-x) (-y)

multiplication (Succ Zero) y = y
multiplication (Succ x   ) y = y + multiplication x y