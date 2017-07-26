module Demo where

{-
Реализуйте функцию, находящую сумму и количество цифр десятичной записи заданного целого числа.

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = undefined
GHCi> sum'n'count (-39)
(12,2)   
-}

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x == 0 = (0,1)
              | otherwise = (numbersSum $ module' x, numberCount $ module' x)
    where 
        module' x = if x < 0 then (-x) else x
        iter'' i pred red acc f = if pred i then iter'' (red i) pred red (f acc i) f else acc
        numbersSum number = 
             iter'' number (\i -> i > 0) (\i -> i `div` 10) (0::Integer) (\acc i -> acc + (i `mod` 10))
        numberCount numer =
             iter'' numer (\i -> i > 0) (\i -> i `div` 10) (0::Integer) (\acc i -> acc + 1)
 

