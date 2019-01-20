module Task4_1 where

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
    fmap x (FunMonad y) = FunMonad (x . y)

instance Applicative FunMonad where
    pure x    = FunMonad (\_ -> x)
    (<*>) x y = appFoo x y

appFoo x y = do
    x1 <- x
    y1 <- y
    return (x1 y1)

instance Monad FunMonad where
    return x             = FunMonad (\_ -> x)
    (>>=) (FunMonad x) y = FunMonad (\z -> (fun $ y $ (x z)) $ z)  