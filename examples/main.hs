module Main where


data Eq' a = MkEq (a -> a -> Bool) (a -> a -> Bool)

eq :: Eq' a -> a -> a -> Bool
eq (MkEq e _) = e

ne :: Eq' a -> a -> a -> Bool
ne (MkEq _ r) = r

dEqInt :: Eq' Int
dEqInt = MkEq (==) (\x y -> not (x == y))

elem' :: Eq' a -> a -> [a] -> Bool
elem' _ _ [] = False
elem' d x (y:ys) = eq d x y || elem' d x ys

main = print $ elem' dEqInt 4 [1..10] 
