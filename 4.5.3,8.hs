module Demo where

{-
Тип List, определенный ниже, эквивалентен определению списков из стандартной библиотеки в том смысле, 
что существуют взаимно обратные функции, преобразующие List a в [a] и обратно. Реализуйте эти функции.
-}

data List a = Nil | Cons a (List a) deriving Show

-- data [] a = [] | a : ([] a) deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x Nil) = [x] 
fromList (Cons a l) = a : (fromList l) ++ []

toList :: [a] -> List a
toList [] = Nil
toList (x:[]) = Cons x Nil
toList (x:xs) = Cons x (toList xs) 


{-
Рассмотрим еще один пример рекурсивного типа данных:

data Nat = Zero | Suc Nat
Элементы этого типа имеют следующий вид: Zero, Suc Zero, Suc (Suc Zero), Suc (Suc (Suc Zero)), и так далее. Таким образом мы можем считать, что элементы этого типа - это натуральные числа в унарной системе счисления.

Мы можем написать функцию, которая преобразует Nat в Integer следующим образом:

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1
Реализуйте функции сложения и умножения этих чисел, а также функцию, вычисляющую факториал.
-}

data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

fromInteger' :: Integer -> Nat
fromInteger' 0 = Zero
fromInteger' n = Suc (fromInteger' (n - 1))

add :: Nat -> Nat -> Nat
add a b = fromInteger' (fromNat a + fromNat b)

mul :: Nat -> Nat -> Nat
mul a b = fromInteger' (fromNat a * fromNat b)

fac :: Nat -> Nat
fac = fromInteger' . fac' . fromNat

fac' :: Integer -> Integer
fac' 0 = 1
fac' n = n * fac' (n - 1)


{-
Тип бинарных деревьев можно описать следующим образом:

data Tree a = Leaf a | Node (Tree a) (Tree a)
 
Реализуйте функцию height, возвращающую высоту дерева, и функцию size, 
возвращающую количество узлов в дереве (и внутренних, и листьев). 
Считается, что дерево, состоящее из одного листа, имеет высоту 0.
-}

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf a) = 0
height (Node a b) = 1 + max (height a) (height b)

size :: Tree a -> Int
size (Leaf a) = 1
size (Node a b) = 1 + size a + size b 

{-
Теперь нам нужно написать функцию avg, которая считает среднее арифметическое всех значений в дереве. И мы хотим, чтобы эта функция осуществляла только один проход по дереву. 
Это можно сделать при помощи вспомогательной функции, возвращающей количество листьев и сумму значений в них. Реализуйте эту функцию.
-}

--data Tree a = Leaf a | Node (Tree a) (Tree a)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf a) = (1, a)
    go (Node a b) = (c1+c2,s1+s2) 
        where
            (c1,s1) = go a
            (c2,s2) = go b


{-
 expand $ v1 * (v2 * (v3 + v4))
Исправьте определение функции expand

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand e1 :*: expand e2
expand e = e
так, чтобы она, используя дистрибутивность (а также, возможно, ассоциативность и коммутативность), всегда возвращала значение, эквивалентное данному и являющееся суммой произведений числовых значений.

GHCi> expand $ (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)
Val 1 :*: Val 4 :+: (Val 1 :*: Val 5 :+: (Val 2 :*: Val 4 :+: (Val 2 :*: Val 5 :+: (Val 3 :*: Val 4 :+: Val 3 :*: Val 5))))
Примечание. Скобки в ответе могут быть расставлены по-другому или вообще отсутствовать, поскольку сложение ассоциативно. Слагаемые могут идти в другом порядке, поскольку сложение коммутативно.
-}


infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr

expand ((e1 :+: e2) :*: e) = if ((e1 :+: e2) :*: e) == expand e1 :*: expand e :+: expand e2 :*: expand e then (expand e1 :*: expand e :+: expand e2 :*: expand e) else expand (expand e1 :*: expand e :+: expand e2 :*: expand e)
expand (e :*: (e1 :+: e2)) = if (e :*: (e1 :+: e2)) == (expand e :*: expand e1 :+: expand e :*: expand e2) then (expand e :*: expand e1 :+: expand e :*: expand e2) else expand (expand e :*: expand e1 :+: expand e :*: expand e2)

expand (e1 :+: e2) = if (e1 :+: e2) == (expand e1 :+: expand e2) then (e1 :+: e2) else expand (expand e1 :+: expand e2)
expand (e1 :*: e2) = if (e1 :*: e2) == (expand e1 :*: expand e2) then  (e1 :*: e2) else expand (expand e1 :*: expand e2)

expand e = e