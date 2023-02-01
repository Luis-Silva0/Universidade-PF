{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# LANGUAGE BlockArguments #-}
import System.Random
import Data.Char (digitToInt)
import Data.List

bingo :: IO ()
bingo = do 
      n <- sorteio []
      print n

sorteio :: [Int] -> IO [Int]
sorteio l | length l == 90 = return l 
          | otherwise = do n <- randomRIO (1,90)
                           print n
                           if elem n l then sorteio l 
                           else sorteio (n:l)

--(b)
mastermind :: IO () 
mastermind = do 
             y <- getLine
             w <- geraJogo []
             x <- fimDeJogo y w
             print x

geraJogo :: [Int] -> IO [Int]
geraJogo l | length l == 4 = return l 
           | otherwise = do 
                         n <- randomRIO (0,9)
                         if elem n l then geraJogo l 
                         else geraJogo (n:l)

fimDeJogo :: String -> [Int] -> IO [String]
fimDeJogo y w = if r == ["Certo","Certo","Certo","Certo"] then return r 
                else do
                     print r
                     z <- getLine
                     fimDeJogo z w
              where r = resultado (map digitToInt y) w

resultado :: [Int] -> [Int] -> [String]
resultado [] l = []
resultado (h:t) (x:y) | h == x = "Certo" : resultado t (y ++ [x])
                      | elem h (x:y) = "Lugar Errado" : resultado t (y ++ [x])
                      | otherwise = "Errado" : resultado t (y ++ [x])  

--2

data Aposta = Ap [Int] (Int,Int)

--(a)
valida :: Aposta -> Bool
valida (Ap l (x,y)) | length (nub l) < 5 = False
                    | (filter (<50) l == l) && (filter (>0) l == l) = (x <=12 && x >=0) && (y<=12 && y>=0)
                    | otherwise = False 

noIntervalo :: (Int,Int) -> Bool
noIntervalo (x,y) = (x <=12 && x >=0) && (y<=12 && y>=0)




