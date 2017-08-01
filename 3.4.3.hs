module Demo where

sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f init []     = init
foldr' f init (x:xs) = x `f` foldr' f init xs

sumList' = foldr' (+) 0 