module Demo where

{-
Реализуйте c использованием функции zipWith функцию fibStream, возвращающую бесконечный список чисел Фибоначчи.

GHCi> take 10 $ fibStream
[0,1,1,2,3,5,8,13,21,34]
-}

fibStream :: [Integer]
fibStream = [0,1] ++ zipWith (+) fibStream (tail fibStream)

{-
Предположим, что функция repeat, была бы определена следующим образом:
repeat = iterate repeatHelper
определите, как должна выглядеть функция repeatHelper.
-}

repeatHelper n = n

{-
Пусть задан тип Odd нечетных чисел следующим образом:

data Odd = Odd Integer 
  deriving (Eq, Show)
Сделайте этот тип представителем класса типов Enum.

GHCi> succ $ Odd (-100000000000003)
Odd (-100000000000001)
Конструкции с четным аргументом, типа Odd 2, считаются недопустимыми и не тестируются.

Примечание. Мы еще не знакомились с объявлениями пользовательских типов данных, однако, скорее всего, приведенное объявление не вызовет сложностей. Здесь объявляется тип данных Odd с конструктором Odd. Фактически это простая упаковка для типа Integer. Часть deriving (Eq, Show) указывает компилятору, чтобы он автоматически сгенерировал представителей соответствующих классов типов для нашего типа (такая возможность имеется для ряда стандартных классов типов). Значения типа Odd можно конструировать следующим образом:

GHCi> let x = Odd 33
GHCi> x
Odd 33
и использовать конструктор данных Odd в сопоставлении с образцом:

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"
-}


-- data Odd = Odd Integer deriving (Eq,Show)
-- не убирайте комментарий с предыдущей строки
-- определение Odd уже присутствует в вызывающей программе

data Odd = Odd Integer deriving (Eq, Show)

instance Enum Odd where
    succ (Odd a) = Odd (a + 2)
    pred (Odd a) = Odd (a - 2)
    toEnum a = Odd (3 + 2 * (toInteger(a) - 1))
    fromEnum (Odd a) = fromIntegral ((a - 1) `div` 2)

    enumFrom = iterate succ
    enumFromThen (Odd a) (Odd b) = 
        map Odd [a, b ..]
    enumFromTo (Odd a) (Odd b) = 
        map Odd [a, a + 2 .. b]
    enumFromThenTo (Odd a) (Odd b) (Odd c) = 
        map Odd [a, b .. c]


addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"

test0 = succ (Odd 1) == (Odd 3)
test1 = pred (Odd 3) == (Odd 1)
-- enumFrom
test2 = (take 3 $ [Odd 1 ..]) == [Odd 1,Odd 3,Odd 5]
-- enumFromTo
-- -- По возрастанию
test3 = (take 3 $ [Odd 1..Odd 7]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test4 = (take 3 $ [Odd 7..Odd 1]) == []
-- enumFromThen
-- -- По возрастанию
test5 = (take 3 $ [Odd 1, Odd 3 ..]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test6 = (take 3 $ [Odd 3, Odd 1 ..]) == [Odd 3,Odd 1,Odd (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 =([Odd 1, Odd 5 .. Odd 7]) == [Odd 1,Odd 5]
-- -- По убыванию
test8 =([Odd 7, Odd 5 .. Odd 1]) == [Odd 7,Odd 5,Odd 3,Odd 1]
-- -- x1 < x3 && x1 > x2
test9 =([Odd 7, Odd 5 .. Odd 11]) == []
-- -- x1 > x3 && x1 < x2
test10 =([Odd 3, Odd 5 .. Odd 1]) == []

allTests = zip [0..] [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]
