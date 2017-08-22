module Demo where

import Data.Char

-- import Prelude hiding (Maybe, Just, Nothing)

-- data Maybe a = Nothing | Just a deriving (Eq, Show)

-- instance Monad Maybe where
--     return = Just

--     (Just x) >>= k = k x
--     Nothing >>= _ = Nothing


{-
Рассмотрим язык арифметических выражений, которые состоят из чисел, скобок, операций сложения и вычитания. Конструкции данного языка можно представить следующим типом данных:

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)
Реализуйте лексер арифметических выражений. Для начала реализуйте следующую функцию:

asToken :: String -> Maybe Token

Она проверяет, является ли переданная строка числом (используйте функцию isDigit из модуля Data.Char), знаком "+" или "-", открывающейся или закрывающейся скобкой. Если является, то она возвращает нужное значение обёрнутое в Just, в противном случае - Nothing:

GHCi> asToken "123"
Just (Number 123)

GHCi> asToken "abc"
Nothing

Далее, реализуйте функцию tokenize:

tokenize :: String -> Maybe [Token]
Функция принимает на вход строку и если каждое слово является корректным токеном, то она возвращает список этих токенов, завёрнутый в Just. В противном случае возвращается Nothing. 

Функция должна разбивать входную строку на отдельные слова по пробелам (используйте библиотечную функцию words). Далее, полученный список строк должен быть свёрнут с использованием функции asToken и свойств монады Maybe:

GHCi> tokenize "1 + 2"
Just [Number 1,Plus,Number 2]

GHCi> tokenize "1 + ( 7 - 2 )"
Just [Number 1,Plus,LeftBrace,Number 7,Minus,Number 2,RightBrace]

GHCi> tokenize "1 + abc"
Nothing
Обратите внимание, что скобки отделяются пробелами от остальных выражений!
-}


data Token = Number Int | Plus | Minus | LeftBrace | RightBrace     
    deriving (Eq, Show)
-- Тип Token уже объявлен, его писать не нужно

asToken :: String -> Maybe Token
asToken t | "+" == t = Just Plus
          | "-" == t = Just Minus
          | "(" == t = Just LeftBrace
          | ")" == t = Just RightBrace
          | isNumber' t == True = Just (Number (read t :: Int))
          | otherwise = Nothing

isNumber' str = all isDigit str

tokenize :: String -> Maybe [Token]
tokenize input = seq (map asToken (words input)) -- sequence
    where
        seq ms = foldr k (return []) ms
        k m1 m2 = do 
                    m1r <- m1
                    m2r <- m2
                    return (m1r:m2r)
                
