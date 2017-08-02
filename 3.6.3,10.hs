module Demo where

unfold :: (b -> (a,b))-> b -> [a]
unfold f init = 
    let (x,init') = f init
    in
        x : unfold f init'

unfoldr :: (b -> Maybe (a,b)) -> b -> [a]
unfoldr f ini = 
    helper (f ini)
    where
        helper Nothing = []
        helper (Just (x, ini')) = x : unfoldr f ini'

{-
Используя unfoldr, реализуйте функцию, которая возвращает в обратном алфавитном порядке список символов, попадающих в заданный парой диапазон. Попадание символа x в диапазон пары (a,b) означает, что x >= a и x <= b.

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g = undefined
GHCi> revRange ('a','z')
"zyxwvutsrqponmlkjihgfedcba"
-}
revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g = (\(a,b) -> if b < a then Nothing else Just (b,(a, pred b)))
