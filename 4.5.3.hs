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