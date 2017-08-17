module Demo where

{-
f :: a -> m b
-- Примеры:
f :: a -> Maybe b            -- m = Maybe
f :: a -> [] b               -- m = []
f :: a -> (Either s) b       -- m = Either s
f :: a -> ((,) s) b          -- m = ((,) s)
f :: a -> ((->) e) b         -- m = ((->) e)
f :: a -> (State s) b        -- m = State s
f :: a -> IO b               -- m = IO
-}

func' :: () -> Bool
func' () = True