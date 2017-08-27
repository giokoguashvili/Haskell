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
    (>>>) :: m a -> m b -> m b

newtype Id a = Id { runId :: a } deriving (Eq, Show)

instance Mond Id where
    return' a = Id a
    Id a >>== k = k a
    Id a >>> idB = idB

wrapSucc :: Integer -> Id Integer
wrapSucc x = Id (succ x) 

-- runId $ wrapSucc 3 >>== return'
{-
1. return a >>= f ≡ f a
2. f >>= return ≡ f
3. f >>= (\x -> g x >>= h) ≡ (f >>= g) >>= h
-}
k = (\a -> Id $ succ a)
law1 = return' 3 >>== k == k 3

m = Id 3
law2 = m >>== return' == m

k' = (\b -> Id $ 2 * b)
law3 = m >>== k >>== k' == m >>== (\b -> k b >>== k')


goWrap0 =
    wrapSucc 3 >>==
    wrapSucc >>==
    wrapSucc >>==
    return'

-- use law2 and write computation in another way
-- wrapSucc 3 >>== wrapSucc >>== wrapSucc 
-- wrapSucc 3 >>== (\x -> wrapSucc x >>== wrapSucc)
goWrap1 = 
    wrapSucc 3 >>== (\x ->
    wrapSucc x >>== (\y ->
    wrapSucc y >>== (\z ->
    return' z)))


goWrap2 = 
    wrapSucc 3 >>== (\x -> -- x := succ 3;
    wrapSucc x >>== (\y -> -- y := succ x;
    wrapSucc y >>== (\z -> -- z := succ y;
    return' (x,y,z))))     -- return (x,y,z)

goWrap3 = 
    wrapSucc 3 >>== \x ->
    wrapSucc x >>== \y ->
    wrapSucc y >>> 
    return' (x,y)

{-
do { 
    e1; 
    e2 
    } 

e1 >> e2
-}

{-
do {
    p <- e1;
    e2;
    }

e1 >>= (\p -> e2)
-}

{-
do {
    let v = e1;
    e2
    }

let v = 
    e1
in
    do { e2 }
-}

{-
1. do {e} → e
2. do {e; es} → e >> do {es}
3. do {let decls; es} → let decls in do {es}
4. do {p <- e; es} → let ok p = do {es} ; ok _ = fail "..." in e >>= ok
-}
goWrap4 = 
    let i = 3 in
    wrapSucc i >>== \x ->
    wrapSucc x >>== \y ->
    wrapSucc y >>> 
    return' (i, x + y)


wrapSucc' x = Just $ succ x
wrapPred' x = Just $ pred x

goWrap5 = do 
    let i = 3 
    x <- wrapSucc' i 
    y <- wrapPred' x
    --wrapSucc' y 
    return (i, x + y)


type Name = String
type DataBase = [(Name,Name)]

fathers, mothers :: DataBase
fathers = [
    ("Bill","John"),
    ("Ann","John"),
    ("John", "Piter")
    ]

mothers = [
    ("Bill","Jane"),
    ("Ann","Jane"),
    ("John","Alice"),
    ("Jane","Dorothy"),
    ("Alice","Mary")
    ]   

getM, getF :: Name -> Maybe Name
getM name = lookup name mothers
getF name = lookup name fathers

getM' = flip lookup $ mothers

granmas :: Name -> Maybe (Name,Name)
granmas name = do
    m <- getM name
    gmm <- getM m

    f <- getF name
    gmf <- getM f
    return (gmm,gmf)


list = [(x,y) | x <- [1,2,3], y <- [4,5,6]]


list' = do
    x <- [1,2,3]
    y <- [4,5,6]
    return (x,y)

list'' =
    [1,2,3] >>= (\x ->
    [4,5,6] >>= (\y ->
    return (x,y)))

lst = [(x,y) | x <- [1,2,3], y <- [1,2], x /= y]

lst' = do
    x <- [1,2,3]
    y <- [1,2]
    True <- return (x/=y)
    return (x,y)

lst'' = 
    [1,2,3]         >>= (\x ->
    [1,2]           >>= (\y ->
    return (x/=y)   >>= (\b ->
    case b of 
        True    -> return (x,y)
        _       -> [])))

lst''' = do
    x <- [1,2,3]
    y <- [1,2]
    if x/=y then
        [True]
    else
        []
    return (x,y)
        