module Demo where

iter n1 acc index n =  if index == n+1 then  acc else iter acc (acc + n1) (index + 1) n
iter' i n acc f = if i == (n + 1) then acc else iter' (i+1) n (f acc i) f 