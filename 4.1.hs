module Demo where

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error =  EQ
cmp Warning Warning =  EQ
cmp Info Info =  EQ
cmp Error _ =  GT
cmp Info _ = LT

cmp Warning Error = LT
cmp Warning Info = GT


