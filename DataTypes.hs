module Demo where

type Endo a = a -> a 

type TypedEndo = Endo (Endo Int) 

f' :: TypedEndo -> Bool
f' = undefined 

f1 :: Endo Int
f1 a = 5

f2 :: TypedEndo 
f2 f1 = f1

-- data AA = Aa Int deriving Show
-- data B = Ba Char deriving Show
-- type C = (A,B)

type IntList = [Int]


data AList = DList [Int]

newtype BListT = BListD [Int] deriving Show

{--}

-- newtype A = A A A
-- newtype A a b = A a b
-- newtype A a = A a a
-- newtype A = A
-- newtype A = A a
-- newtype A a = A

-- + newtype A a b = A b
-- + newtype A a b = A a
-- + newtype A = A A
-- + newtype A a = A a

newtype Id a = Id { run::a } deriving Show