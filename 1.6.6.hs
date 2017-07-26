module Demo where
{-
Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности

a0=1;a1=2;a2=3;ak+3=ak+2+ak+1−2ak.
a0=1;a1=2;a2=3;ak+3=ak+2+ak+1−2ak.
Попытайтесь найти эффективное решение.

GHCi> seqA 301
1276538859311178639666612897162414
-}  
seqA :: Integer -> Integer
seqA n | n == 0 = 1
       | n == 1 = 2
       | n == 2 = 3
       | otherwise = 
           let 
               formula k k1 k2 = k2 + k1 - 2*k
               iter k k1 k2 index p = 
                    if index == (p + 1) then 
                        k2 
                    else 
                        iter k1 k2 (formula k k1 k2) (index + 1) p 
           in 
               iter 1 2 3 3 n