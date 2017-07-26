module Demo where

sum a b = a + b

fibonacci :: Integer -> Integer
fibonacci n | (n < 0) = if n `mod` 2 == 0 then (-1) * (iter 0 1 2 (-n)) else (iter 0 1 2 (-n))
            | n == 0      = 0
            | n == 1      = 1
            | otherwise   = iter 0 1 2 n

iter n1 acc index n =  if index == n+1 then  acc else iter acc (acc + n1) (index + 1) n


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

iter' i n acc f = if i == (n + 1) then acc else iter' (i+1) n (f acc i) f 

-- iter'' i pred red acc f = if pred i then iter'' (red i) pred red (f acc i) f else acc

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x == 0 = (0,1)
              | otherwise = (numbersSum $ module' x, numberCount $ module' x)
    where 
        module' x = if x < 0 then (-x) else x
        iter'' i pred red acc f = if pred i then iter'' (red i) pred red (f acc i) f else acc
        numbersSum number = 
             iter'' number (\i -> i > 0) (\i -> i `div` 10) (0::Integer) (\acc i -> acc + (i `mod` 10))
        numberCount numer =
             iter'' numer (\i -> i > 0) (\i -> i `div` 10) (0::Integer) (\acc i -> acc + 1)
 

