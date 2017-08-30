module Demo where

data DA = Da { contentA::String }
data DB = Db { contentB::String }

data DC = DA | DB

data TypeA = TA 

class TypeClassA a where

class TypeClassB b where

instance TypeClassA TypeA where

instance TypeClassB TypeA where