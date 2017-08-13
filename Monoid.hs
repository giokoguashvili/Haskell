module Demo where

import Prelude hiding (lookup)
import qualified Data.List as L

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


{-
Ниже приведено определение класса MapLike типов, похожих на тип Map. 
Определите представителя MapLike для типа ListMap, определенного ниже как список пар ключ-значение. 
Для каждого ключа должно храниться не больше одного значения.
Функция insert заменяет старое значение новым, если ключ уже содержался в структуре.
-}

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] } deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap []
    
    lookup _ (ListMap []) = Nothing
    lookup k (ListMap ((k',v'):xs)) = if k' == k then Just v' else lookup k (ListMap xs)

    delete _ (ListMap []) = ListMap []
    delete k (ListMap ((k',v'):xs)) = if k' == k then ListMap xs else ListMap ((k',v'): getListMap (delete k (ListMap xs)))
       
    
    insert k v lm = ListMap (sort $ (insert' k v lm))
        where 
            sort (ListMap xs) = L.sortBy (\(k1,_) (k2,_) -> k2 `compare` k1) xs
            
            insert' k v (ListMap []) = ListMap ((k,v):[])
            insert' k v (ListMap ((k',v'):xs)) = if k == k' then ListMap ((k,v):xs) else ListMap ((k',v'):getListMap (insert k v (ListMap xs)))
   