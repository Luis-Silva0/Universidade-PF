{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}

import Data.Char

perimetro:: Float -> Float 
perimetro r = 2 * pi * r

-- comentário

-- tudo o que tem a haver com tipos começa com letra maiúscula

type Hora = (Int,Int)

 -- todos os nomes de funções e argumentos começam por minúscula


meiaNoiteEUmQuarto :: Hora
meiaNoiteEUmQuarto = (0,15)

duasMenosUmQuarto :: Hora
duasMenosUmQuarto = (13,45)

horaValida :: Hora -> Bool
horaValida (h,m) = (h>= 0 && h<=23)&&(m>=0 && m<=59)


-- true quando a 1a é maior que a 2a
comparaHoras :: Hora -> Hora -> Bool
comparaHoras (h1,m1) (h2,m2) = 
   if h1 >h2 then True
   else if h1 == h2 then m1 > m2
        else False


comparaHorasB:: Hora -> Hora -> Bool
comparaHorasB (h1,m1) (h2,m2) | h1 > h2 = True
                              | h1 == h2 = m1 > m2
                              | h1 < h2 = False


horasParaMinutos :: Hora -> Int
horasParaMinutos (hora,minutos) = hora * 60 + minutos

minutosParaHoras :: Int -> Hora 
minutosParaHoras minutos = ( div minutos 60 , mod minutos 60 )

diferencaHoras :: Hora -> Hora -> Int
diferencaHoras h1 h2 = abs (horasParaMinutos h1 - horasParaMinutos h2)

adicionaMinutos :: Int -> Hora -> Hora 
adicionaMinutos m h = minutosParaHoras ( horasParaMinutos h + m )

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

e :: Extracto
e = Ext 200 [(D 5 4 2010,"deposito",Credito 2000),(D 10 8 2010, "compra", Debito 37.5)]

instance Show Data where
    show (D d m a) = show d ++ "/" ++ show m ++ "/" ++ show a

instance Show Extracto where 
    show (Ext n l) = "Saldo anterior:" ++ show n ++
                     "\n-------------------------------------------------" ++
                     "\nData       Descricao   Credito   Debito" ++
                     "\n-------------------------------------------------\n" ++ concatMap (\(dat,str,mov) -> case mov of Credito n -> show dat ++ replicate (11- (length (show dat ))) ' ' ++ map (toUpper) str ++ replicate (12 - length (map (toUpper) str)) ' ' ++ show n ++"    \n"
                                                                                                                         Debito n -> show dat ++ replicate (11- (length (show dat ))) ' ' ++ map (toUpper) str ++ replicate (22 - length (map (toUpper) str)) ' ' ++ show n ++"    \n") l ++
                     "--------------------------------------------------" ++
                     "\nSaldo actual:" ++ show (saldo (Ext n l))

saldo :: Extracto -> Float
saldo (Ext a l) = foldl (\acc (_,_,mov) -> case mov of Credito n -> (acc + n)
                                                       Debito n -> (acc - n)) a l






