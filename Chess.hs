{-# LANGUAGE UnicodeSyntax #-}
module Board
    ( initialBoardStr
    ) where
import Data.Char
import Control.Monad
b = ' '
w = '█'
type Ve = Int
type Ho = Int

type Board = [String]

initialBoardStr :: [String] -- c c c 
initialBoardStr = ["♖♘♗♕♔♗♘♖","♙♙♙♙♙♙♙♙"]++ (take 4 $ repeat "        ") ++ ["♟♟♟♟♟♟♟♟","♜♞♝♛♚♝♞♜"]

logicSquare :: Ho → Ve → Char → String
logicSquare h v ' '
  | odd v = [if odd h then b else w]
  | otherwise = [if odd h then w else b]
logicSquare h v cc
  | odd v = [if odd h then cc else cc]
  | otherwise = [if odd h then cc else cc]

squareFree c = [c,c,c]
squareWithFig = cmc
cmc c m = [c,m,c]

showLine:: Int → String → String
showLine nl ss= concatMap (\ (h,cc) -> logicSquare h (8-nl) cc) $ zip [1..] ss

printBoard :: Board → IO ()
printBoard bb = do
        forM_ [0..7] (\i → do
            putStrLn $ show (8- i) ++" "++ showLine i (bb !! i))
        putStrLn $ "  "++ ['a'..'h']-- concatMap (cmc b)