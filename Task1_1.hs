module Task1_1 where

import Todo(todo)

data Operacii = Plus        -- +
                | Minus     -- -
                | Multiply  -- *
                deriving (Show,Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term, opr :: Operacii } -- бинарная операция
            deriving (Show,Eq)


-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l r Plus

(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l r Minus

(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l r Multiply

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
        Variable v             -> if v == varName then replacement else expression
        BinaryTerm lhv rhv opr -> BinaryTerm (replaceVar varName replacement lhv) (replaceVar varName replacement rhv) 
                                  opr
        _                      -> expression  

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = 
    case expression of
        BinaryTerm lhv rhv opr -> 
            case (left, right, opr) of
                (IntConstant 0,    IntConstant right, Multiply) -> IntConstant 0
                (IntConstant 1,    IntConstant right, Multiply) -> IntConstant right
                (IntConstant left, IntConstant 0,     Multiply) -> IntConstant 0
                (IntConstant left, IntConstant 1,     Multiply) -> IntConstant left
                (IntConstant left, IntConstant 0,     Plus)     -> IntConstant left
                (IntConstant 0,    IntConstant right, Plus)     -> IntConstant right
                (IntConstant left, IntConstant right, Plus)     -> IntConstant (left + right)
                (IntConstant left, IntConstant right, Multiply) -> IntConstant (left * right)
                (IntConstant left, IntConstant right, Minus)    -> IntConstant (left - right)
                _ -> BinaryTerm left right opr
            where
                left  = evaluate(lhv)
                right = evaluate(rhv)
        _ -> expression
    




