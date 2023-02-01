{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use record patterns" #-}
{-# HLINT ignore "Redundant if" #-}

module Ficha1 where
import Data.Time (hoursToTimeZone)
import Data.Char
-- Exercício 1

perimetro:: Float -> Float 
perimetro r = 2 * pi * r

dist :: Double -> Double -> Double
dist x y = abs (x-y)

primUlt :: [Int] -> (Int,Int)
primUlt x = (head x,last x)

multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0

truncaImpar :: [Int] -> [Int]
truncaImpar x |mod (length x) 2 == 0 = x
              |otherwise = tail x

max2 :: Int -> Int -> Int
max2 x y | x > y = x
         |otherwise = y  

max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z

-- Exercício 2
            
nRaizes :: Double -> Double -> Double -> Int
nRaizes x y z |(y * y - 4 * x * z) > 0 = 2
              |(y * y - 4 * x * z) == 0 = 1
              |otherwise = 0

raizes :: Double -> Double -> Double -> [Double]
raizes x y z |nRaizes x y z == 0 = []
             |nRaizes x y z == 1 = [(-y) / 2*x]
             |otherwise = [((-y) + (y * y - 4 * x * z)) / 2*x,(y + (y * y - 4 * x * z)) / 2*x]




-- comentário

-- tudo o que tem a haver com tipos começa com letra maiúscula

--type Hora = (Int,Int)
--
-- -- todos os nomes de funções e argumentos começam por minúscula
--
--
--meiaNoiteEUmQuarto :: Hora
--meiaNoiteEUmQuarto = (0,15)
--
--duasMenosUmQuarto :: Hora
--duasMenosUmQuarto = (13,45)
--
--horaValida :: Hora -> Bool
--horaValida (h,m) = (h>= 0 && h<=23)&&(m>=0 && m<=59)
--
--
---- true quando a 1a é maior que a 2a
--comparaHoras :: Hora -> Hora -> Bool
--comparaHoras (h1,m1) (h2,m2) = 
--   if h1 >h2 then True
--   else if h1 == h2 then m1 > m2
--        else False
--
--
--comparaHorasB:: Hora -> Hora -> Bool
--comparaHorasB (h1,m1) (h2,m2) | h1 > h2 = True
--                              | h1 == h2 = m1 > m2
--                              | h1 < h2 = False
--
--
--horasParaMinutos :: Hora -> Int
--horasParaMinutos (hora,minutos) = hora * 60 + minutos
--
--minutosParaHoras :: Int -> Hora 
--minutosParaHoras minutos = ( div minutos 60 , mod minutos 60 )
--
--diferencaHoras :: Hora -> Hora -> Int
--diferencaHoras h1 h2 = abs (horasParaMinutos h1 - horasParaMinutos h2)
--
--adicionaMinutos :: Int -> Hora -> Hora 
--adicionaMinutos m h = minutosParaHoras ( horasParaMinutos h + m )

data Hora = H Int Int deriving (Show,Eq)

horaValida :: Hora -> Bool
horaValida (H x y) = (x >= 0 && x <= 23) && (y >= 0 && y <= 59) 

comparaHoras :: Hora -> Hora -> Bool
comparaHoras (H x1 y1) (H x2 y2) |x1 > x2 = True
                                 |x1 == x2 = y1 > y2
                                 |otherwise = False

horasParaMinutos :: Hora -> Int
horasParaMinutos (H x y) = x*60 + y 

minutosParaHoras :: Int -> Hora
minutosParaHoras x = H (div x 60) (mod x 60)

diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (H x1 y1) (H x2 y2) = abs (horasParaMinutos (H x1 y1) - horasParaMinutos (H x2 y2))

adicionaMinutos :: Int -> Hora -> Hora
adicionaMinutos x (H y z) = minutosParaHoras (horasParaMinutos (H y z) + x)


data Semaforo = Verde
              | Amarelo
              | Vermelho
              deriving (Show,Eq)



next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

stop :: Semaforo -> Bool
stop Vermelho = True
stop _        = False

-- falso se ambos os semáforos estiverem verdes
safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = not ((s1 == Verde) && (s2 == Verde))

-- Exercício 6

data Ponto = Cartesiano Double Double
           | Polar Double Double
           deriving (Show,Eq)

--(a) 
posx :: Ponto -> Double
posx (Cartesiano x _) = abs x
posx (Polar x y) = abs (x * sin y)

--(b)
posy :: Ponto -> Double 
posy (Cartesiano x y) = y
posy (Polar x y) = abs (x * cos y)

--(c)
raio :: Ponto -> Double
raio (Polar x y) = abs x
raio (Cartesiano x y) = sqrt ((x^2) + (y^2))

--(d)
angulo :: Ponto -> Double
angulo (Polar x y) = y 
angulo (Cartesiano x y) = asin (posy (Cartesiano x y)/raio (Cartesiano x y))

--(e)
distA :: Ponto -> Ponto -> Double
distA x y = sqrt ((posx x - posx y)^2 - (posy x - posy y)^2)

--7
data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)

--(a)
poligono :: Figura -> Bool
poligono x = case x of
    (Circulo _ _) -> False
    (Rectangulo _ _) -> True
    (Triangulo _ _ _) -> True

--(b)
vertices :: Figura -> [Ponto]
vertices (Circulo x y) = []
vertices (Rectangulo x y) = [x,y]
vertices (Triangulo x y z) = [x,y,z]

--(c)
area :: Figura -> Double
area (Triangulo p1 p2 p3) =
       let a = distA p1 p2
           b = distA p2 p3
           c = distA p3 p1
           s = (a+b+c) / 2 -- semi-perimetro
       in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Rectangulo p1 p2) = 
       let a = posx p1
           b = posx p2
           c = posy p1 
           d = posy p2 
       in abs (b - a) * (d - c) 
area (Circulo p r) = pi * r^2

--(d)
perimetroA :: Figura -> Double
perimetroA (Triangulo p1 p2 p3) =
       let a = distA p1 p2
           b = distA p2 p3
           c = distA p3 p1
       in a+b+c
perimetroA (Rectangulo p1 p2) = 
             let a = posx p1
                 b = posx p2
                 c = posy p1 
                 d = posy p2 
             in abs (2*(b-a) + 2*(d-c))
perimetroA (Circulo p r) = 2*pi*r

--8
--(a)
isLowerC :: Char -> Bool
isLowerC x = if 97 <= ord x then ord x <= 122
            else False

--(b)
isDigitC :: Char -> Bool
isDigitC x = (48 <= ord x) && (ord x <= 57)

--(c)
isAlphaC :: Char -> Bool
isAlphaC x | 65 >= ord x = ord x <= 90
           | 97 >= ord x = ord x <= 122
           | otherwise = False 

--(d)
toUpperC :: Char -> Char
toUpperC x = if ord x <= 90 then x 
            else chr (ord x - 32)

--(e)













