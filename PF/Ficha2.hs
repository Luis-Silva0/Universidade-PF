{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Redundant if" #-}
import Data.Char


--2
--(a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2*h : dobros t

dobrosEff :: [Float] -> [Float]
dobrosEff = map (2*)

--(b)
numOcorre :: Char -> String -> Int
numOcorre x [] = 0 
numOcorre x (h:t) = if x == h then 1 + numOcorre x t 
                    else numOcorre x t 

--(c)
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if h <= 0 then False
                  else positivos t

--(d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h > 0 then h : soPos t
              else soPos t 

--(e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if h <= 0 then h + somaNeg t 
                else somaNeg t 

--(f)
tresUlt :: [a] -> [a]
tresUlt l = if length l <= 3 then l 
            else tresUlt (tail l)

--(g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):t) = y : segundos t 

--(h)
nosPrimeiros :: Eq a => a -> [(a,b)] -> Bool
nosPrimeiros x [] = False
nosPrimeiros x ((y,z):t) = if x == y then True
                           else nosPrimeiros x t

--(i)
sumTriplos :: (Num a,Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) = (x+a,y+b,z+c)
                      where (a,b,c) = sumTriplos t

--3
--(a)
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if (48 <= ord h) && (ord h <= 57) then h : soDigitos t 
                  else soDigitos t 

--(b)
minisculas :: [Char] -> Int
minisculas [] = 0
minisculas (h:t) = if (97 <= ord h) && (122 >= ord h) then 1 + minisculas t 
                   else minisculas t 

--(c)
nums :: String -> [Int]
nums [] = []
nums (h:t) = if (48 <= ord h) && (ord h <= 57) then digitToInt h : nums t 
                  else nums t

--4
type Polinomio = [Monomio]
type Monomio = (Float,Int)

p1 :: Polinomio
p1 = [(2,3), (3,4), (5,3), (4,5)]

p2 :: Polinomio
p2 = [(4,8), (1,2), (7,6), (4,1)]

--(a)
conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((x,y):t) = if y == n then 1 + conta n t
                    else conta n t

--(b)
grau :: Polinomio -> Int
grau [] = 0
grau ((x,y):t) = max y b
        where b = grau t

--(c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n ((x,y):t) = if y == n then (x,y) : selgrau n t
                    else selgrau n t

--(d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,y):t) = (x * fromIntegral y,y-1) : deriv t

--(e)
calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x ((y,z):t) = (y * (x^z)) + calcula x t 

--(f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((x,y):t) = if y == 0 then simp t 
                 else (x,y) : simp t

--(g)
mult :: Monomio -> Polinomio -> Polinomio
mult x [] = []
mult (x,y) ((w,z):t) = (x*w,y+z) : mult (x,y) t

--(h)
normaliza :: Polinomio -> Polinomio
normaliza [(b,e)] = [(b,e)]
normaliza ((b, e) : (b2, e2) : ps) | e == e2 = normaliza ((b + b2, e) : ps)
                                   | conta e ps == 0 = (b, e) : normaliza ((b2, e2) : ps)
                                   | otherwise = normaliza ((b, e) : ps ++ [(b2, e2)])

--(i)
soma :: Polinomio -> Polinomio -> Polinomio
soma l [] = l
soma [] l = l 
soma p1 p2 = normaliza (p1 ++ p2)

--(j)
produto :: Polinomio -> Polinomio -> Polinomio
produto l [] = l 
produto [] l = l 
produto ((x,y):t) p2 = normaliza (mult (x,y) p2 ++ produto t p2)

--(k)
ordena :: Polinomio -> Polinomio
ordena [(b,e)] = [(b,e)]
ordena ((b,e):(b2,e2):t) = if e <= e2 then (b,e) : ordena ((b2,e2):t)
                           else ordena ((b2,e2) : t ++ [(b,e)])

--(l)
equiv :: Polinomio -> Polinomio -> Bool
equiv [] [] = True
equiv p1 p2 = if ordena (normaliza p1) == ordena (normaliza p2) then True
              else False

              

















