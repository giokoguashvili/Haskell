module Demo where
    
{-
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
-}

{-
1.  return a >>= k == k a
2.  m >>= return == m
3.  ?! (m >>= k) >>= k' == m >>= (k >>= k')
                           m >>= ((\x -> k x) >>= k')
-}


{-
f :: a -> m b
-- Примеры:
f :: a -> Maybe b            -- m = Maybe
f :: a -> [] b               -- m = []
f :: a -> (Either s) b       -- m = Either s
f :: a -> ((,) s) b          -- m = ((,) s)
f :: a -> ((->) e) b         -- m = ((->) e)
f :: a -> (State s) b        -- m = State s
f :: a -> IO b               -- m = IO
-}

{-
Введём следующий тип:

data Log a = Log [String] a
Реализуйте вычисление с логированием, используя Log. Для начала определите функцию toLogger

toLogger :: (a -> b) -> String -> (a -> Log b)
которая превращает обычную функцию, в функцию с логированием:

GHCi> let add1Log = toLogger (+1) "added one"
GHCi> add1Log 3
Log ["added one"] 4

GHCi> let mult2Log = toLogger (* 2) "multiplied by 2"
GHCi> mult2Log 3
Log ["multiplied by 2"] 6
Далее, определите функцию execLoggers

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c

Которая принимает некоторый элемент и две функции с логированием. execLoggers возвращает результат последовательного применения функций к элементу и список сообщений, которые были выданы при применении каждой из функций:
GHCi> execLoggers 3 add1Log mult2Log
Log ["added one","multiplied by 2"] 8
-}


data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = (\a -> Log (msg:[]) (f a))

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = 
            Log (fxs ++ gxs) ga
            where
                (Log fxs fa) = f x
                (Log gxs ga) = g fa

{-
Функции с логированием из предыдущего задания возвращают в качестве результата значение с некоторой дополнительной информацией в виде списка сообщений. Этот список является контекстом. Реализуйте функцию returnLog

returnLog :: a -> Log a

которая является аналогом функции return для контекста Log. Данная функция должна возвращать переданное ей значение с пустым контекстом.
-}

returnLog :: a -> Log a
returnLog a = Log [] a 

{-
Реализуйте фукцию bindLog

bindLog :: Log a -> (a -> Log b) -> Log b
которая работает подобно оператору >>= для контекста Log.

GHCi> Log ["nothing done yet"] 0 `bindLog` add1Log
Log ["nothing done yet","added one"] 1

GHCi> Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
Log ["nothing done yet","added one","multiplied by 2"] 8
-}

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log ls x) f = Log (ls ++ fls) fx
                    where
                        (Log fls fx) = f x

{-
Реализованные ранее returnLog и bindLog позволяют объявить тип Log представителем класса Monad:

instance Monad Log where
    return = returnLog
    (>>=) = bindLog
Используя return и >>=, определите функцию execLoggersList

execLoggersList :: a -> [a -> Log a] -> Log a
которая принимает некоторый элемент, список функций с логированием и возвращает результат последовательного применения всех функций в списке к переданному элементу вместе со списком сообщений, которые возвращались данными функциями:

GHCi> execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
Log ["added one","multiplied by 2","multiplied by 100"] 800
-}

-- instance Monad Log where
--     return = returnLog
--     (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a = foldl (\acc next -> bindLog acc next) (returnLog a)


{-
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    x >> y = x >>= (\_ -> y)


(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) f g x = g x >>= f
-}


-- newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

-- instance Monad Identity where
--     return = Identity
--     Identity a >>= f = f a




{-
Используя монаду списка и do-нотацию, реализуйте функцию

pythagoreanTriple :: Int -> [(Int, Int, Int)]

которая принимает на вход некоторое число xx и возвращает список троек (a,b,c)(a,b,c), таких что

a2+b2=c2,a>0,b>0,c>0,c≤x,a<ba2+b2=c2,a>0,b>0,c>0,c≤x,a<b  

Число xx может быть ≤0≤0 , на таком входе должен возвращаться пустой список.

GHCi> pythagoreanTriple 5
[(3,4,5)]

GHCi> pythagoreanTriple 0
[]

GHCi> pythagoreanTriple 10
[(3,4,5),(6,8,10)]
-}


pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple 0 = []
pythagoreanTriple x = do 
                        c <- [1..x]
                        a <- [1..x]
                        b <- [1..x]
                        if (a*a + b*b == c*c) && (a<b) then "A" else []
                        return (a,b,c)