--1.
--(a)
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use lambda" #-}

intersectC :: Eq a => [a] -> [a] -> [a]
intersectC [] l = []
intersectC (x:xs) l = if elem x l then x : intersectC xs l 
                      else intersectC xs l

--(b)

tailsC :: [a] -> [[a]]
tailsC [] = [[]]
tailsC (x:xs) = (x:xs) : tailsC xs

--2.

type ConjInt = [Intervalos]
type Intervalos = (Int,Int)

--(a)
elemsC :: ConjInt -> [Int]
elemsC [] = []
elemsC ((x,y):t) = if x == y then x : elemsC t
                   else x : elemsC ((x+1,y):t)

--(c)

geraConjC :: [Int] -> ConjInt
geraConjC [] = []
geraConjC (x:xs) = (x,a) : geraConjC (dropWhile (<= a) xs)
                 where a = foldl (\ acc x -> if x == succ acc then x else acc ) x xs

--3.
data Contacto = Casa Integer 
              | Trab Integer
              | Tlm Integer
              | Email String
        deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]

--(a)

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail x y l = l ++ [(x,[Email y])] 

--(b)

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [(x,c)] = if n == x then Just (map (\ x -> case x of Email e -> e ) c ) else Nothing 
verEmails n ((x,c):agenda) = if n == x then Just (map (\ x -> case x of Email e -> e ) c ) else verEmails n agenda

--(c)

consulta :: [Contacto] -> ([Integer],[String])
consulta [] = ([],[])
consulta (x:xs) = case x of
            Casa x -> (x:a,b)
            Trab x -> (x:a,b)
            Tlm x -> (x:a,b)
            Email x -> (a,x:b)
        where (a,b) = consulta xs

--(d)

consultaIO :: Agenda -> IO ()
consultaIO ((n,c):xs) = do x <- getLine
                           let contactos = aux x ((n,c):xs)
                           putStr (concat [show x ++ "\n" | x <- contactos])

                        where aux _ [] = []
                              aux x ((n,c):xs) = if x == n then c 
                                                 else aux x xs

--4.
data RTree a = R a [RTree a] deriving (Show,Eq)
--(a)

paths :: RTree a -> [[a]]
paths (R z []) = [[z]]
paths (R node branches) = [node : x | x <- concat [ paths branch | branch <- branches]]

--(b)

unpaths :: Eq a => [[a]] -> RTree a
unpaths [[x]] = R x [] 
unpaths [x:xs] = R x [unpaths [xs]]
unpaths ((x:xs):l) = R x (unpaths [xs]: map unpaths [l])


