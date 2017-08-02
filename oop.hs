module Demo where

data Person = Person { firstName::String, lastName::String, age::Int } deriving Show


fullName :: Person -> ((a,a) -> String)
fullName p =
    let f (,) = firstName p
    in f



infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x