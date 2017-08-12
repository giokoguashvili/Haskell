module Demo where
import Data.List.Split
import Data.Char


-- import Data.Time.Clock
-- import Data.Time.Format
-- import System.Locale

-- timeToString :: UTCTime -> String
-- timeToString = formatTime defaultTimeLocale "%a %d %T"

-- data LogLevel = Error | Warning | Info deriving Show

-- data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

-- logLevelToString :: LogLevel -> String
-- logLevelToString = show

-- logEntryToString :: LogEntry -> String
-- logEntryToString le = (timeToString $ (timestamp le)) ++ ": " ++ (logLevelToString $ (logLevel le)) ++ ": " ++ (message le)

data Shape = Circle Double | Rectangle Double Double
isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False


data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

abbrFirstName :: Person -> Person
abbrFirstName p = 
    if (length $ firstName p) <= 2 then
        p 
    else
        p { firstName = ((head $ firstName p) : ".") }


{-
Реализуйте функцию parsePerson, которая разбирает строки вида firstName = John\nlastName = Connor\nage = 30 и возвращает либо результат типа Person, либо ошибку типа Error.

Строка, которая подается на вход, должна разбивать по символу '\n' на список строк, каждая из которых имеет вид X = Y. Если входная строка не имеет указанный вид, то функция должна возвращать ParsingError.
Если указаны не все поля, то возвращается IncompleteDataError.
Если в поле age указано не число, то возвращается IncorrectDataError str, где str — содержимое поля age.
Если в строке присутствуют лишние поля, то они игнорируются.
-}


data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

--data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
parsePerson str = if isValidStr str == False then
                        Left ParsingError
                  else if (hasAllFields fields pairs') == False then
                        Left IncompleteDataError
                  else if ageIsCorrect str == False then
                        Left (IncorrectDataError $ snd (valueOf "age" pairs'))
                  else 
                        Right Person { 
                                    lastName = snd $ valueOf "lastName" pairs',
                                    firstName = snd $ valueOf "firstName" pairs',
                                    age = (read (snd $ valueOf "age" pairs')::Int)
                                }
                  where
                    pairs' = pairs str
                    fields = ["lastName", "firstName", "age"]




pairs str = map (\(key:value:[]) -> (key,value)) (filter isPair (map keyValues (fields str)))
isPair str = length str == 2
fields = splitOn "\n"
keyValues str = let 
                s1 = splitOn " = " str
                s2 = splitOn " =" str
            in 
                if (length s1) == 0 then
                    s2
                else
                    s1
isValidStr str = (length $ pairs str) > 0 && isValidFields str && (length (snd (age' str)) > 0)
valueOf key pairs = head (filter (\(key',_) -> key' == key) pairs)
isNumber' str = all (True ==) (map isDigit str) && length str > 0

hasAllFields fields pairs = all (True ==) (map (\f -> hasField f pairs) fields)
hasField field pairs = length (filter (\(key,_) -> key == field) pairs) > 0

age' str = valueOf "age" (pairs str)
ageIsCorrect str = isNumber' (snd (age' str))
isValidFields str = all (True==) (map (\f -> ((length (splitOn "=" f)) > 0) && ((length (splitOn " = " f)) > 0)) (fields str))



-- wrong Parse | empty string
t0 = parsePerson ""
-- correct
t1 = parsePerson "firstName = John\nlastName = Connor\nage = 30"
-- wrong Parse | no spaces around = in minor fields
t2 = parsePerson "firstName = John Smith\nlastName = Connor\nage = 30\nasde=as11"
-- wrong Parse | no spaces around = in major fields
t3 = parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30"
 -- wrong Incorrect | age is non-numeric
t4 = parsePerson "firstName = John\nlastName = Connor\nage = as30"
-- wrong Parse | no spaces around = on the left in minor fields
t5 = parsePerson "firstName = John Smith\nlastName = Connor\nage = 30\nasde= "
-- wrong Parse | no spaces around = in major fields, missing major field
t6 = parsePerson "firstName=Barbarian\nlastName=Conn Or"
-- wrong Parse | no spaces around = in major fields, typo in major field
t7 = parsePerson "firstNameee = John Smith\nlastName = Connor\nage = 30\nasde=as11"
-- correct | excessive fields
t8 = parsePerson "firstName = John\nlastName = Connor\nfoo = bar\nage = 30"
-- wrong Incomplete | missing major field
t9 = parsePerson "firstName = Barbarian\nlastName = Conn Or"

-- wrong Parse | empty major value
t10 = parsePerson "firstName = John\nlastName = Connor\nage = "
-- wrong Parse | no spaces around = on the right in major field
t11 = parsePerson "firstName = John\nlastName = Connor\nage ="
-- wrong Parse | empty key, missing major field
t12 = parsePerson "firstName = John\nlastName = Connor\n = 30"
-- correct | spaces in major field value
t13 = parsePerson "firstName = Barbarian\nlastName = Conn On\nage = 30"
-- correct | = in major field value
t14 = parsePerson "firstName = John\nlastName = Con=nor\nage = 30"
-- wrong Parse | no spaces around =, missing value in minor field
t15 = parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30\ng dsfsd"
-- wrong Incomplete | major field key with whitespace, age is non-numeric
t17 = parsePerson " firstName = John\nlastName = Connor\nage = 2f8 "
-- correct | shiffled fields
t18 = parsePerson "lastName = Connor\nfirstName = John\nage = 30"

