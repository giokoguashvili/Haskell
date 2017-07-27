module Demo where
{-
Реализуйте функцию, находящую значение определённого интеграла от заданной функции ff на заданном интервале [a,b][a,b] методом трапеций. (Используйте равномерную сетку; достаточно 1000 элементарных отрезков.)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = undefined
GHCi> integration sin pi 0
-2.0
Результат может отличаться от -2.0, но не более чем на 1e-4.
-}

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = 
    integration' f a b 1000 
    where  
        integration' f a b n = 
            h * (f a + f b) / 2  + h * sum 1 (n-1) f xi
            where 
                iter'' i pred red acc f = if pred i then iter'' (red i) pred red (f acc i) f else acc
                sum i n f xi =
                    iter'' i (\i -> i <= n) (\i -> i + 1) (0::Double) 
                        (\acc i -> acc + f (xi i))
                h = (b - a) / n
                xi i = a + i * h  
        ---(h / 2) * (f (xi 0) + 2 * sum 1 (n-1) f xi + f (xi n))
