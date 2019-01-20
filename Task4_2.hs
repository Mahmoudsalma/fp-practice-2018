module Task4_2 where
import Control.Monad

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что 
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
    fmap = liftM

instance Applicative FourOf where
    pure y = FourOf y y y y
    (<*>)  = ap

instance Monad FourOf where
    return y                 = FourOf y y y y
    (>>=) (FourOf a b c d) y = FourOf (get1(y a)) (get2(y b)) (get3(y c)) (get4(y d))
        where get1 (FourOf a b c d) = a 
              get2 (FourOf a b c d) = b 
              get3 (FourOf a b c d) = c 
              get4 (FourOf a b c d) = d 
