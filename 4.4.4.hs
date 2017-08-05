module Demo where


data Coord a = Coord a a deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter a (Coord x y) =
       Coord (cord a x) (cord a y)
       where 
            cord a xy = a * (fromIntegral xy) + half
            half = a / 2  

getCell :: Double -> Coord Double -> Coord Int
getCell a (Coord x y)= Coord (fl x a) (fl y a)
            where
                fl xy a = floor (xy / a)