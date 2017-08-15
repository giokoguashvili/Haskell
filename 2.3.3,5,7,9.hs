module Demo where
    
class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString(x) = if x == True then "true" else "false"

instance Printable () where
    toString() = "unit type"

instance (Printable a, Printable b) => Printable (a,b) where
    toString (a,b) = "(" ++ toString a ++ "," ++ toString b ++ ")"


class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x  | doesEnrageMork x && doesEnrageGork x = stomp (stab x)
                   | doesEnrageGork x = stab x
                   | doesEnrageMork x = stomp x
                   | otherwise = x

{-
Реализуйте класс типов

class SafeEnum a where
  ssucc :: a -> a
  spred :: a -> a
обе функции которого ведут себя как succ и pred стандартного класса Enum, однако являются тотальными, то есть не останавливаются с ошибкой на наибольшем и наименьшем значениях типа-перечисления соответственно, а обеспечивают циклическое поведение. Ваш класс должен быть расширением ряда классов типов стандартной библиотеки, так чтобы можно было написать реализацию по умолчанию его методов, позволяющую объявлять его представителей без необходимости писать какой бы то ни было код. Например, для типа Bool должно быть достаточно написать строку

instance SafeEnum Bool
и получить возможность вызывать

GHCi> ssucc False
True
GHCi> ssucc True
False
-}
class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a | a == maxBound = minBound
          | otherwise = succ a

  spred :: a -> a
  spred a | a == minBound = maxBound
          | otherwise = pred a

instance SafeEnum Bool where
instance SafeEnum Char where

func :: (SafeEnum b) => b -> b
func b = spred b

func' b = spred b