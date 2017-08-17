module Demo where

data Person = Person { firstName::String, lastName::String, age::Int } deriving Show

-- let person = Person { firstName = "Gio", lastName = "Kogo", age = 25 }
-- person & fullName()
fullName :: () -> Person -> String
fullName () p = (firstName p) ++ " " ++ (lastName p)

infixl 1 .!
(.!) :: a -> (a -> b) -> b
x .! f = f x

data PersonType a b = PersonData { f::a, f2::a, f3::b}
