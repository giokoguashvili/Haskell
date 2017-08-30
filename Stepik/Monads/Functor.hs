module Demo where
    
{-
https://en.wikibooks.org/wiki/Haskell/The_Functor_class
-}

{-
class Functor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a
-}

{-
Laws:
    id :: a -> a
    id = \x -> x

    1. fmap id == id
    2. fmap (f . g) == fmap f . fmap g
       fmap (f . g) F == fmap f (fmap g F)
-}

{- 
instance Functor (Either e) where
    fmap _ left = left
    fmap g (Right y) = Right $ g y

instance Functor ((,) s) where
    fmap g (x, y) = (x, g y)
-}

_F = Just 3
f = (+1)
g = (*2)

law1 = fmap id _F == id _F 
law2 = fmap (f . g) _F == fmap f (fmap g _F) 