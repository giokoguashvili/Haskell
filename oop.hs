module Demo where

data Person = Person { firstName::String, lastName::String, age::Int } deriving Show


-- fullName :: Person -> ((a,a) -> String)
-- fullName p =
--     let f (,) = firstName p
--     in f



infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

data PersonType a b = PersonData { f::a, f2::a, f3::b}
