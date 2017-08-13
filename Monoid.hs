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


{-
Реализуйте представителя класса типов Monoid для Maybe' a так, чтобы mempty 
не был равен Maybe' Nothing. Нельзя накладывать никаких дополнительных ограничений на тип a, 
кроме указанных в условии.
-}

newtype Maybe' a = Maybe' { getMaybe :: Maybe a } deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' $ Just mempty
    mappend _ (Maybe' (Nothing)) = Maybe' $ Nothing
    mappend (Maybe' (Nothing)) _ = Maybe' $ Nothing
    mappend (Maybe' x) (Maybe' y) = Maybe' $ mappend x y