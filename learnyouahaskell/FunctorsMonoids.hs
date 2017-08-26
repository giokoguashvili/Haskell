module Demo where


data TypeA = TA 

class TypeClassA a where

class TypeClassB b where

instance TypeClassA TypeA where

instance TypeClassB TypeA where