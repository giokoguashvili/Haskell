module Demo where

{-
Определите представителя класса Functor для следующего типа данных, представляющего точку в трёхмерном пространстве:

data Point3D a = Point3D a a a deriving Show

GHCi> fmap (+ 1) (Point3D 5 6 7)
Point3D 6 7 8
-}

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)


{-
Определите представителя класса Functor для типа данных GeomPrimitive, который определён следующим образом:

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)
При определении, воспользуйтесь тем, что Point3D уже является представителем класса Functor.

GHCi> fmap (+ 1) $ Point (Point3D 0 0 0)
Point (Point3D 1 1 1)

GHCi> fmap (+ 1) $ LineSegment (Point3D 0 0 0) (Point3D 1 1 1)
LineSegment (Point3D 1 1 1) (Point3D 2 2 2)
-}
data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
    fmap f (LineSegment a b) = LineSegment (fmap f a) (fmap f b)
    fmap f (Point a) = Point $ fmap f a


{-

Определите представителя класса Functor для бинарного дерева, в каждом узле которого хранятся элементы типа Maybe:

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

GHCi> words <$> Leaf Nothing
Leaf Nothing

GHCi> words <$> Leaf (Just "a b")
Leaf (Just ["a","b"]

-}

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show
instance Functor Tree where
    fmap f (Leaf Nothing) = Leaf Nothing
    fmap f (Leaf (Just a)) = Leaf (Just (f a))
    fmap f (Branch l (Nothing) r) = Branch (fmap f l) Nothing (fmap f r)
    fmap f (Branch l (Just a) r) = Branch (fmap f l) (Just (f a)) (fmap f r)