module Demo where

data Person = Person { firstName::String, lastName::String, age::Int } deriving Show


-- fullName :: Person -> ((a,a) -> String)
-- fullName p =
--     let f (,) = firstName p
--     in f



infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

{-
TypeClass
-}
class ClassType box where
    doSomething :: (a,b) -> (box a) b

data XType a b = XData {first::a, second::b} deriving Show

instance ClassType XType where
    doSomething (a, b) = XData a b