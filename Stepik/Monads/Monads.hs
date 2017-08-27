module Demo where

{-
class Applicative m => Monad (m :: * -> *) where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>= (\_ -> y)


    fail :: String -> m a
-}

{-
1.  return a >>= k == k a
2.  m >>= return == m
3.  ?! (m >>= k) >>= k' == m >>= (k >>= k')
                           m >>= (\x -> k x >>= k')
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
computational context
-}

toKleisli :: Monad m => (a -> b) -> (a -> m b)
toKleisli fab = (\a -> return $ fab a)

{-
:t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b

:t flip fmap
flip fmap :: Functor f => f a -> (a -> b) -> f b
-}


{-
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

(<=<) :: Monad m => (b -> mc) -> (a -> m b) - (a -> m c)
f <=< g = \a -> g a >>= f
-}

class Mond m where
    return' :: a -> m a
    (>>==) :: m a -> (a -> m b) -> m b

newtype Id a = Id { runId :: a } deriving (Eq, Show)

instance Mond Id where
    return' a = Id a
    Id a >>== k = k a

wrapSucc :: Integer -> Id Integer
wrapSucc x = Id (succ x) 

-- runId $ wrapSucc 3 >>== return'

k = (\a -> Id $ succ a)
law1 = return' 3 >>== k == k 3

m = Id 3
law2 = m >>== return' == m

k' = (\b -> Id $ 2 * b)
law3 = m >>== k >>== k' == m >>== (\b -> k b >>== k')