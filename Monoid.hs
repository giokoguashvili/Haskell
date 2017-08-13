module Demo where


class Monoid' a where
    mempty' :: a
    mappend' :: a -> a -> a
    mconcat' :: [a] -> a
    mconcat' = foldr mappend' mempty'


instance Monoid' [a] where
    mempty' = []
    mappend' = (++)


{-
Реализуйте представителя класса типов Monoid для типа Xor, в котором mappend выполняет операцию xor.
-}
newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty = Xor False
    mappend a b   | getXor a == True && getXor b == True = Xor False 
                  | otherwise = Xor ((getXor a) || (getXor b))
