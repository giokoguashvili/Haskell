module Demo where

import Data.Function

getSecondFrom :: a1 -> b1 -> c1 -> b1
getSecondFrom a b c = b


multSecond = g `on` h

g = (+)

h = snd


doItYourself = f . g . h
    where 
        f = logBase 2
        g = (^) 3
        h = max 42

let funcs = (curry, uncurry, flip, (,), const)

let v1 (a,_,_,_,_) = a
let v2 (_,a,_,_,_) = a
let v3 (_,_,a,_,_) = a
let v4 (_,_,_,a,_) = a
let v5 (_,_,_,_,a) = a


iter fns fn = 
    fn (v1 fns) 
    fn (v2 fns)
    fn (v3 fns)
    fn (v4 fns)
    fn (v5 fns)
    undefined

let run =
    iter funs (\f1 -> iter funs (\f2 -> iter funs (\f3 -> f1(f2 f3)))))