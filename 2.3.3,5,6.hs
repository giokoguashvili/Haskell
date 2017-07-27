module Demo where
    
class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString(x) = if x == True then "true" else "false"

instance Printable () where
    toString() = "unit type"


instance (Printable a, Printable b) => Printable (a,b) where
    toString (a,b) = "(" ++ toString a ++ "," ++ toString b ++ ")"


