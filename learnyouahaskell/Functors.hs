module Demo where

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