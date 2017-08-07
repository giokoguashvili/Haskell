module Demo where

type Endo a = a -> a 

type TypedEndo = Endo (Endo Int) 

f' :: TypedEndo -> Bool
f' = undefined 

f1 :: Endo Int
f1 a = 5

f2 :: TypedEndo 
f2 f1 = f1