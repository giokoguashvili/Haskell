module Demo where

data B = T | F deriving (Show, Eq, Read, Enum, Ord)

not' :: B -> B
not' T = F
not' F = T

data Color = Red | Green | Blue 

instance Show Color where
   show Red = "Red"
   show Green = "Green"
   show Blue = "Blue"