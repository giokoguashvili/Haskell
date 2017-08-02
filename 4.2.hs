module Demo where

{-
Целое число можно представить как список битов со знаком.

Реализуйте функции сложения и умножения для таких целых чисел, считая, что младшие биты идут в начале списка, а старшие — в конце. Можно считать, что на вход не будут подаваться числа с ведущими нулями. 
-}

data Bit = Zero | One deriving (Eq, Show)
data Sign = Minus | Plus deriving (Eq, Show)
data Z = Z Sign [Bit] deriving (Eq, Show)

add :: Z -> Z -> Z
add a b = integerToZ ((zToInteger a) + (zToInteger b))

mul :: Z -> Z -> Z
mul a b = integerToZ ((zToInteger a) * (zToInteger b))

zToInteger (Z sign bytes) =  
        if sign == Minus then (-1) * (integer bytes) else integer bytes
        where 
            bytesArray bytes' = zip (map (\b -> if b == Zero then 0 else 1) bytes') [0..]
            number = foldr (\(b,i) ini -> ini + b * (2^i)) 0 
            integer = number . bytesArray 


integerToZ number = 
    if number > 0 then
        Z Plus (result number)
    else if number == 0 then
        Z Plus []
    else
        Z Minus (result ((-1) * number))
    where 
        intToBytes 0 = [0]
        intToBytes n = (n `mod` 2) : intToBytes (n `div` 2)
        bytesToZ = map (\x -> if x == 0 then Zero else One) 
        result n = reverse $ bytesToZ $ (dropWhile (\x -> x == 0) (reverse (intToBytes n)))

-- intToBytes 0 = [0]
-- intToBytes n = (n `mod` 2) : intToBytes (n `div` 2)
bytesArray bytes' = zip (map (\b -> if b == Zero then 0 else 1) bytes') [0..]
number = foldr (\(b,i) ini -> ini + b * (2^i)) 0 
integer = number . bytesArray 

emptyZ = Z Plus []
test001 = (add (Z Plus []) (Z Plus [])) == Z Plus []
test002 = (add (Z Plus []) (Z Plus [One])) == Z Plus [One]
test003 = (add (Z Plus []) (Z Minus [One])) == Z Minus [One]

test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]
test012 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One])) == Z Plus [Zero, Zero, Zero, One]
test013 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus [Zero, Zero, One, One]

test021 = (add (Z Minus [Zero, One, One]) (Z Minus [One])) == Z Minus [One, One, One]
test022 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One])) == Z Minus [Zero, Zero, Zero, One]
test023 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One, One]

test031 = (add (Z Minus [Zero, One, One]) (Z Plus [One])) == Z Minus [One, Zero, One]
test032 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One])) == Z Minus [Zero, Zero, One]
test033 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus []

test041 = (add (Z Plus [Zero, One, One]) (Z Minus [One])) == Z Plus [One, Zero, One]
test042 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One])) == Z Plus [Zero, Zero, One]
test043 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Plus []

test051 = (add (Z Plus [One]) (Z Minus [One])) == Z Plus []
test052 = (add (Z Plus [One]) (Z Minus [One, One])) == Z Minus [Zero, One]
test053 = (add (Z Plus [One]) (Z Minus [Zero, One])) == Z Minus [One]
test054 = (add (Z Plus [One]) (Z Minus [Zero, Zero, Zero, One])) == Z Minus [One, One, One]
test055 = (add (Z Plus [One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, Zero, One]
test056 = (add (Z Plus [Zero, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One]
test057 = (add (Z Plus [Zero, One]) (Z Minus [Zero, Zero, One])) == Z Minus [Zero, One]
test058 = (add (Z Plus [One, Zero, One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, One]


test101 = (mul (Z Plus []) (Z Plus [])) == emptyZ
test102 = (mul (Z Plus []) (Z Plus [One])) == emptyZ
test103 = (mul (Z Plus []) (Z Minus [One])) == emptyZ
test104 = (mul (Z Plus [One]) (Z Plus [])) == emptyZ
test105 = (mul (Z Minus [One]) (Z Plus [])) == emptyZ

test111 = (mul (Z Plus [One]) (Z Plus [One])) == Z Plus [One]
test112 = (mul (Z Minus [One]) (Z Plus [One])) == Z Minus [One]
test113 = (mul (Z Plus [One]) (Z Minus [One])) == Z Minus [One]
test114 = (mul (Z Minus [One]) (Z Minus [One])) == Z Plus [One]

test121 = (mul (Z Plus [One]) (Z Plus [Zero, One])) == Z Plus [Zero, One]
test122 = (mul (Z Plus [Zero, Zero, One]) (Z Plus [Zero, Zero, One])) == Z Plus [Zero, Zero, Zero, Zero, One]

test131 = (mul (Z Plus [One, Zero, One, Zero, One]) (Z Plus [One, One, One])) == Z Plus [One, One, Zero, Zero, One, Zero, Zero, One]


testAdd = test001 && test002 && test003 && test011 && test012 && test013 && test021 && test022 && test023 && test031 && test032 && test033 && test041 && test042 && test043 && test051 && test052 && test053 && test054 && test055 && test056 && test057 && test058
testMul = test101 && test102 && test103 && test104 && test105 && test111 && test112 && test113 && test114 && test121 && test122 && test131

testAll = testAdd && testMul

tests' = [(test001,001), (test002,002), (test003,003), (test011,011), (test012,012), (test013,013), (test021,021), (test022,022), (test023,023), (test031,031), (test032,032), (test033,033), (test041,041), (test042,042), (test043,043), (test051,051), (test052,052), (test053,053), (test054,054), (test055,055), (test056,056), (test057,057), (test058,058), (test101,101), (test102,102), (test103,103), (test104,104), (test105,105), (test111,111), (test112,112), (test113,113), (test114,114), (test121,121), (test122,122), (test131,131)]
fTests = filter (\(r, n) -> r == False) tests' 