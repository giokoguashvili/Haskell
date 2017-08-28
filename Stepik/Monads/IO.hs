module Demo where

    {-
        https://wiki.haskell.org/IO_inside
        http://conscientiousprogrammer.com/blog/2015/12/11/24-days-of-hackage-2015-day-11-monad-loops-avoiding-writing-recursive-functions-by-refactoring/
    -}

    main = getLine >>= (\str -> putStrLn $ str ++ "!") 

    {-
        1.
            getchar :: Char
            get2chars = [getChar, getChar]
        
        2.
            getchar :: Int -> Char
            get2chars :: Int -> String

            get2chars _ = [getchar 1, getchar 2]

        3.
            getchar :: Int -> (Char, Int)
            get2chars _ = [a,b] 
                        where 
                                (a,i) = getchar 1
                                (b,_) = getchar i

        4.
            get2chars i0 = [a,b]
                        where 
                            (a,i1) = getchar i0
                            (b,i2) = getchar i1
            get4chars = [get2chars 1, get2chars 2] -- same problem

        5.
            get2chars :: Int -> (String, Int)
            get4chars i0 = (a++b)
                        where
                            (a,i1) = get2char i0
                            (b,i2) = get2char i1
        
        6.
            get2chars :: Int -> (String, Int)
            get2chars i0 = ([a,b], i2)
                    where
                        (a,i1) = getchar i0
                        (b,i2) = getchar i1
    
    -}


    {-
        1.
            newtype IO a = IO (RealWord -> (RealWord, a))

            main :: RealWorld -> ((), RealWorld)
            type IO a  =  RealWorld -> (a, RealWorld)

        2.
            main :: IO ()
            getChar :: IO Char

            main w0 = 
                let
                    (ch1, w1) = getChar w0
                    (ch2, w2) = getChar w1
                in
                    ((), w2)

    -}

    getLine' :: IO String
    getLine' = do
        ch <- getChar
        if ch == '\n' then
            return []
        else do
            chs <- getLine'
            return (ch:chs)

    putStr' :: String -> IO ()
    putStr' [] = return ()
    putStr' (ch:chs) = putChar ch >> putStr' chs

    seq_ :: (Monad m, Foldable t) => t (m a) -> m ()
    seq_ = foldr (>>) (return ())

    putStrLn' :: String -> IO ()
    putStrLn' str = seq_ . map putChar $ str ++ "\n"