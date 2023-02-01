{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# HLINT ignore "Use infix" #-}

import Ficha1


--(1)
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

v1 :: Viagem
v1 = [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]

--(a)
etapaVal :: Etapa -> Bool
etapaVal (x,y) = comparaHoras x y

--(b)
viagemVal :: Viagem -> Bool
viagemVal [] = True
viagemVal (x:y:t) = if (etapaVal x && etapaVal y) then comparaHoras xt yh && viagemVal (y:t)
                    else False
          where (xh,xt) = x
                (yh,yt) = y

--(c)
partidaChegada :: Viagem -> (Hora,Hora)
partidaChegada [(x,y)] = (x,y)
partidaChegada v = (x,ys)
          where (x,xs) = head v
                (y,ys) = last v

--(d)
tempoViagem :: Viagem -> Int
tempoViagem [] = 0
tempoViagem ((x,xs):t) = diferencaHoras x xs + tempoViagem t 

--(e)
tempoEspera :: Viagem -> Int
tempoEspera [] = 0
tempoEspera v = diferencaHoras x y - tempoViagem v 
          where (x,y) = partidaChegada v 

--(f)
tempoTotal :: Viagem -> Int
tempoTotal v = tempoEspera v + tempoViagem v 

--(2)
type Poligonal = [Ponto]

po1 :: Poligonal
po1 = [(Cartesiano 1 3),(Cartesiano 9 7),(Cartesiano 5 4)]

po2 :: Poligonal
po2 = [(Cartesiano 1 3),(Cartesiano 9 7),(Cartesiano 1 3)]

--(a)
comprimento :: Poligonal -> Double
comprimento [] = 0
comprimento [x] = 0
comprimento (x:y:t) = distA x y + comprimento (y:t)

--(b)
verFechado :: Poligonal -> Bool
verFechado p = if head p == last p then True
               else False

--3
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
            deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

--(a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail x y [] = [(x,[Email y])]
acrescEmail x y l = l ++ [(x,[Email y])]

--(b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails x [] = Nothing
verEmails x ((y,(z:zs)):t) = if x == y then Just (emails (z:zs))
                             else verEmails x t

emails :: [Contacto] -> [String]
emails [] = []
emails (x:xs) = case x of
    Email y -> y : emails xs
    otherwise -> emails xs

--(c)
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (h:t) = case h of 
    Email a -> consTelefs t 
    Casa a -> a : consTelefs t
    Trab a -> a : consTelefs t
    Tlm a -> a : consTelefs t

--(d)
casa :: Nome -> Agenda -> Maybe Integer
casa x [] = Nothing
casa x ((y,(z:zs)):t) = if x == y then Just (numCasa (z:zs))
                        else casa x t

numCasa :: [Contacto] -> Integer
numCasa [] = 0
numCasa (x:xs) = case x of
    Casa y -> y
    otherwise -> numCasa xs

--4
{-
type Dia = Int
type Mes = Int
type Ano = Int

data Data = D Dia Mes Ano
          deriving Show

type TabDN = [(Nome,Data)]

t1 :: TabDN
t1 = [("Luis",D 04 07 2004),("Carla",D 26 05 1976),("Severino",D 02 06 1973)]

--(a)
procura :: Nome -> TabDN -> Maybe Data
procura x [] = Nothing
procura x ((y,z):t) = if x == y then Just z 
                      else procura x t

--(b)
idade :: Data -> Nome -> TabDN -> Maybe Int
idade x y [] = Nothing
idade (D x y z) w ((h,(D a b c)):t) | w == h && ((y > b) || (y == b && x > a) || (y == b && x == a)) = Just (z-c)
                                    | w == h && ((y > b) || (y==b && x < a)) = Just (z-c-1)
                                    | otherwise = idade (D x y z) w t

--(c)
anterior :: Data -> Data -> Bool    
anterior (D d m a) (D d1 m1 a1) | (a1 > a) || (a1 == a && m1 > m) || (a1 == a && m1 == m && d1 > d) = True
                                | otherwise = False

--(d)
ordena :: TabDN -> TabDN 
ordena [x] = [x]
ordena ((x,y):(z,w):t) = if anterior y w then (x,y) : ordena ((z,w):t)
                         else ordena  ((z,w):t) ++ [(x,y)]

--(e)
porIdade:: Data -> TabDN -> [(Nome,Int)]
porIdade x [] = []
porIdade (D d m a) l = (n,idade) : porIdade (D d m a) ts
          where ((n,(D dx mx ax)):ts) = ordena l 
                idade = if (m > mx) || (mx == m && d > dx) then (a-ax)
                        else ((a-ax)-1)
-}
--5

data Movimento = Credito Float 
               | Debito Float
               deriving Show

data Data = D Int Int Int
          deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
              deriving Show

--(a)
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) _ = []
extValor (Ext x1 ((a,b,c):t)) x2 | x1 > x2 = c : extValor (Ext x1 t) x2
                                 | otherwise = extValor (Ext x1 t) x2

--(b)
filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ l) [] = []
filtro (Ext _ []) l = []
filtro (Ext x ((a,b,c):t)) l = if elem b l then (a,c) : filtro (Ext x t) l 
                               else filtro (Ext x t) l 

--(c)
creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext x ((a,b,c):t)) = case c of
             Credito y -> (y+a,b)
             Debito z -> (a,b+z)
            where (a,b) = creDeb (Ext x t)

--(d)
saldo :: Extracto -> Float
saldo (Ext x []) = x 
saldo (Ext x ((a,b,c):t)) = x + y + z 
             where y = fst (creDeb (Ext x ((a,b,c):t)))
                   z = snd (creDeb (Ext x ((a,b,c):t)))

                                                          



                        







