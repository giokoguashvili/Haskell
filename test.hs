module Demo where

iter n1 acc index n =  if index == n+1 then  acc else iter acc (acc + n1) (index + 1) n
iter' i n acc f = if i == (n + 1) then acc else iter' (i+1) n (f acc i) f 

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (a:as) =
     a:take (n-1) as

drop' :: Int -> [a] -> [a]
drop' 0 as = as
drop' _ [] = []
drop' n (_:as) =  
     drop (n-1) as 

splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n as = (take' n as, drop' n as)

xs !!! n | n < 0 = error "ups"
[] !!! _ = error "ups"
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (a:as) | p a = a : filter' p as 
                 | otherwise = filter' p as
                
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (a:as) 
    | p a = a : takeWhile' p as
    | otherwise = []