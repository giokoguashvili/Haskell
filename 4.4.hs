module Demo where

import Data.List.Split
import Data.Char
{-
Реализуйте функцию parsePerson, которая разбирает строки вида firstName = John\nlastName = Connor\nage = 30 и возвращает либо результат типа Person, либо ошибку типа Error.

Строка, которая подается на вход, должна разбивать по символу '\n' на список строк, каждая из которых имеет вид X = Y. Если входная строка не имеет указанный вид, то функция должна возвращать ParsingError.
Если указаны не все поля, то возвращается IncompleteDataError.
Если в поле age указано не число, то возвращается IncorrectDataError str, где str — содержимое поля age.
Если в строке присутствуют лишние поля, то они игнорируются.
-}
data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

parsePerson :: String -> Either Error Person
parsePerson = asPerson . validatedAge . validatedData . mappedPairs . validatedPairs . parsedPairs . validatedLines . parsedLines



asPerson :: Either Error [(String,String)] -> Either Error Person
asPerson (Left a) = Left a
asPerson (Right mps) =
        Right (Person {
            lastName = keyValue "lastName",
            firstName = keyValue "firstName",
            age = read $ keyValue "age"
        })
        where 
            keyValue = valueFromPairWithKey mps


validatedAge :: Either Error [(String,String)] -> Either Error [(String,String)]
validatedAge (Left a) = Left a
validatedAge (Right mps) =
            if isNumber' ageValue then
                Right mps
            else
                Left (IncorrectDataError ageValue)
            where 
                ageValue = valueFromPairWithKey mps "age" 

isNumber' str = all (True ==) (map isDigit str) && length str > 0

pairByKey mps key = head (filter (\(key',_) -> key' == key) mps)
valueFromPairWithKey mps key = snd $ pairByKey mps key

validatedData :: Either Error [(String,String)] -> Either Error [(String,String)]
validatedData (Left a) = Left a
validatedData (Right mps) =
            if all (True ==) (map (\k -> containsPairWithKey mps k) keys) then
                Right mps
            else
                Left IncompleteDataError

keys = ["firstName", "lastName", "age"]

containsPairWithKey :: [(String,String)] -> String -> Bool
containsPairWithKey mps key = length (filter (\(key',_) -> key' == key) mps) /= 0

mappedPairs :: Either Error [[String]] -> Either Error [(String,String)]
mappedPairs (Left a) = Left a
mappedPairs (Right vp) = Right $ map mapper vp
                where 
                    mapper vp = (vp!!0, vp!!1)


validatedPairs :: Either Error [[String]] -> Either Error [[String]]
validatedPairs (Left a) = Left a
validatedPairs (Right pairs) = 
    if all (True==) (map validatedPair pairs) then
        Right pairs
    else
        Left ParsingError

parsedPairs :: Either Error [String] -> Either Error [[String]]
parsedPairs (Left a) = Left a
parsedPairs (Right p) = Right (map parsedPairAsList p)

parsedPairAsList ::  String -> [String]
parsedPairAsList = splitOn " = "

validatedPair :: [String] -> Bool
validatedPair pairs = if length pairs == 2 && (head pairs) /= "" && (pairs !! 1) /= "" then True else False 
        

parsedLines :: String -> [String]
parsedLines = splitOn "\n"

validatedLines :: [String] -> Either Error [String] 
validatedLines lines =
    if isEmpty lines then
        Left ParsingError
    else
        Right lines
    where
        isEmpty strs = if length strs == 1 && head strs == "" then True else False

--(asPerson . validetedAge . validatedData . mappedPairs . validatedPairs . parsedPairs . validatedLines . parsedLines) "firstName = John\nlastName = Connor\nage = 1.2"


-- import Data.List.Split
-- import Data.Char

-- data Error = ParsingError | IncompleteDataError | IncorrectDataError String

-- data Person = Person { firstName :: String, lastName :: String, age :: Int }

-- parsePerson :: String -> Either Error Person
-- parsePerson str = if isValidStr str == False then
--                         Left ParsingError
--                   else if (hasAllFields fields pairs') == False then
--                         Left IncompleteDataError
--                   else if ageIsCorrect str == False then
--                         Left (IncorrectDataError $ snd (valueOf "age" pairs'))
--                   else 
--                         Right Person { 
--                                     lastName = snd $ valueOf "age" pairs',
--                                     firstName = snd $ valueOf "firstName" pairs',
--                                     age = (read (snd $ valueOf "age" pairs')::Int)
--                                 }
--                   where
--                     pairs' = pairs str
--                     fields = ["lastName", "firstName", "age"]




-- pairs str = map (\(key:value:[]) -> (key,value)) (filter isPair (map keyValues (fields str)))
-- isPair str = length str == 2
-- fields = splitOn "\n"
-- keyValues = splitOn " = " 
-- isValidStr str = (length $ pairs str) > 0 && isValidFields str && (length (snd (age' str)) > 0)
-- valueOf key pairs = head (filter (\(key',_) -> key' == key) pairs)
-- isNumber' str = all (True ==) (map isDigit str) && length str > 0

-- hasAllFields fields pairs = all (True ==) (map (\f -> hasField f pairs) fields)
-- hasField field pairs = length (filter (\(key,_) -> key == field) pairs) > 0

-- age' str = valueOf "age" (pairs str)
-- ageIsCorrect str = isNumber' (snd (age' str))
-- isValidFields str = all (True==) (map (\f -> ((length (splitOn "=" f)) > 0) && ((length (splitOn " = " f)) > 0)) (fields str))

{-
sequence
words
lines
lookup
elem

data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

parseDictionary :: String -> Maybe [(String, String)]
parseDictionary = sequence . map (parseSet . words) . lines where
    parseSet [n,"=",v] = Just (n,v)
    parseSet _         = Nothing

parsePerson :: String -> Either Error Person
parsePerson s = case parseDictionary s of
    Nothing -> Left ParsingError
    Just d  -> case lookup "firstName" d of
        Nothing -> Left IncompleteDataError
        Just fn -> case lookup "lastName" d of
            Nothing -> Left IncompleteDataError
            Just ln -> case lookup "age" d of
               Nothing -> Left IncompleteDataError
               Just ag -> case all (`elem` ['0'..'9']) ag of
                   False -> Left  $ IncorrectDataError ag
                   True  -> Right $ Person { firstName = fn, lastName = ln, age = read ag }
-}