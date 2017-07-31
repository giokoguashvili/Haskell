module Demo where

{-
Реализуйте функцию addTwoElements, которая бы добавляла два переданных ей значения в голову переданного списка.

GHCi> addTwoElements 2 12 [85,0,6]
[2,12,85,0,6]
-}

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements fst snd ls = fst : snd : ls


{-
Реализуйте функцию nTimes, которая возвращает список, состоящий из повторяющихся значений ее первого аргумента. Количество повторов определяется значением второго аргумента этой функции.

GHCi> nTimes 42 3
[42,42,42]
GHCi> nTimes 'z' 5
"zzzzz"
-}

nTimes:: a -> Int -> [a]
nTimes a count = 
    nTimes' a count
    where
        nTimes' a 0 = []
        nTimes' a count =
            a : nTimes' a (count - 1)


second :: [a] -> a
second = (head.tail)

fst' ((,) x y) = x

head' ((:) x xs) = x

{-
Сформируйте список целых чисел, содержащий только те элементы исходного списка, значение которых нечетно.

GHCi> oddsOnly [2,5,7,10,11,12]
[5,7,11]
Для анализа четности можно использовать функции odd и even стандартной библиотеки.
-}

oddsOnly :: Integral a => [a] -> [a]
oddsOnly xs =
        addsOnly' 2 xs
        where 
            addsOnly' x [] =  if isOdd x then [x] else []
            addsOnly' x xs =
                if isOdd x then x : addsOnly' (head xs) (tail xs) else addsOnly' (head xs) (tail xs)
            isOdd x = if x `mod` 2 == 0 then False else True

{-
Реализуйте функцию isPalindrome, которая определяет, является ли переданный ей список палиндромом.

GHCi> isPalindrome "saippuakivikauppias"
True
GHCi> isPalindrome [1]
True
GHCi> isPalindrome [1, 2]
False
-}

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs | reverse' xs == xs = True
                | otherwise = False
                where
                    reverse' xs = rev xs [] where
                        rev [] a = a
                        rev (x:xs) a = rev xs (x:a)


{-
Составьте список сумм соответствующих элементов трех заданных списков. Длина результирующего списка должна быть равна длине самого длинного из заданных списков, при этом «закончившиеся» списки не должны давать вклада в суммы.

GHCi> sum3 [1,2,3] [4,5] [6]
[11,7,3]
-}                        
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 [] (x:as) (y:bs) = (x + y) : (sum3 [] as bs)
sum3 (x:as) [] (y:bs) = (x + y) : (sum3 as [] bs)
sum3 (x:as) (y:bs) [] = (x + y) : (sum3 as bs [])
sum3 [] [] (x:as) = x : (sum3 [] [] as)
sum3 [] (x:as) [] = x : (sum3 [] as [])
sum3 (x:as) [] [] = x : (sum3 as [] [])
sum3 (x:as) (y:bs) (z:cs) = 
    (x + y + z) : (sum3 as bs cs)


{-
Напишите функцию groupElems которая группирует одинаковые элементы в списке (если они идут подряд) и возвращает список таких групп.

GHCi> groupElems []
[]
GHCi> groupElems [1,2]
[[1],[2]]
GHCi> groupElems [1,2,2,2,4]
[[1],[2,2,2],[4]]
GHCi> groupElems [1,2,3,2,4]
[[1],[2],[3],[2],[4]]
Разрешается использовать только функции, доступные из библиотеки Prelude.
-}

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems [a] = [a] : []
groupElems (a:as) = 
    let (:) ((:) b bs) bss = groupElems as
    in
        if a == b then (a:b:bs):bss else (a:[]):((b:bs):bss)
