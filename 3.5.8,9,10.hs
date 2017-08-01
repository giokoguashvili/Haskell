module Demo where

{-
Реализуйте функцию meanList, которая находит среднее значение элементов списка, используя однократный вызов функции свертки.

GHCi> meanList [1,2,3,4]
2.5
Постобработка считается допустимой, то есть предполагаемая реализация функции meanList имеет вид

meanList = someFun . foldr someFoldingFun someIni
-}

meanList :: [Double] -> Double
meanList = 
    mean' . (foldr (\x (s,c) -> (x+s,c+1)) (0,0))
    where 
        mean' (s,c) = s / c

{-
Используя однократный вызов свертки, реализуйте функцию evenOnly, которая выбрасывает из списка элементы, стоящие на нечетных местах, оставляя только четные.

GHCi> evenOnly [1..10]
[2,4,6,8,10]
GHCi> evenOnly ['a'..'z']
"bdfhjlnprtvxz"
-}

evenOnly :: [a] -> [a]
evenOnly = result . foldl (\(xs,i) x-> if i `mod` 2 == 1 then (x:xs,i+1) else (xs,i+1)) ([],0)
        where 
            result (xs, c) = reverse xs

{-
Попробуйте добиться того, чтобы реализованная вами в прошлом задании функция evenOnly позволяла работать и с бесконечными списками.
То есть, например, запрос на первые три элемента бесконечного списка, возвращаемого этой функцией, примененной к списку всех натуральных чисел, должен завершаться:

GHCi> take 3 (evenOnly [1..])
[2,4,6]
-}

venOnly :: [a] -> [a]
venOnly xs = 
    evenOnly' xs 0
    where
        evenOnly' [] _ = []
        evenOnly' (x:xs) i = if mod i 2 == 1 then x:(evenOnly' xs (i+1)) else (evenOnly' xs (i+1))