-- 50 questões
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use :" #-}
import Data.Char ()
import Data.List ( delete, insert )

-- 1 Apresente uma definição recursiva da função (pré-definida) enumFromTo :: Int -> Int -> [Int] que constrói a lista dos números inteiros compreendidos entre dois limites.
enumFromToB :: Int -> Int -> [Int]
enumFromToB x y = if x==y then [x]
                  else x : enumFromToB (x+1) y

-- 2 Apresente uma definição recursiva da função (pré-definida) enumFromThenTo :: Int -> Int -> Int -> [Int] que constrói a lista dos números inteiros compreendidos entre dois limites e espa¸cados de um valor constante.
enumFromThenToB :: Int -> Int -> Int -> [Int]
enumFromThenToB x y z = if y > z then [x]
                        else x : enumFromThenToB (x+(y-x)) (y+(y-x)) z

-- 3 Apresente uma definição recursiva da função (pré-definida) (++) :: [a] -> [a] -> [a] que concatena duas listas.
juntaListas :: [a] -> [a] -> [a]
juntaListas x [] = x
juntaListas [] x = x
juntaListas (x:xs) y = x : juntaListas xs y

-- 4 Apresente uma definição recursiva da função (pré-definida) (!!) :: [a] -> Int -> a que dada uma lista e um inteiro, calcula o elemento da lista que se encontra nessa posição (assume-se que o primeiro elemento se encontra na posição 0).
identificaElm :: [a] -> Int -> a
identificaElm x y = if y==0 then head x
                         else identificaElm (tail x) (y-1)

-- 5 Apresente uma definição recursiva da função (pré-definida) reverse :: [a] -> [a] que dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa.
reverseB :: [a] -> [a]
reverseB [] = []
reverseB (x:y)= reverseB y ++ [x]

-- 6 Apresente uma definição recursiva da função (pré-definida) take :: Int -> [a] -> [a] que dado um inteiro n e uma lista l calcula a lista com os (no máximo) n primeiros elementos de l.
takeB :: Int -> [a] -> [a]
takeB x [] = []
takeB x (h:t) = if x==0 then []
                else h : takeB (x-1) t

-- 7  Apresente uma definicão recursiva da função (pré-definida) drop :: Int -> [a] -> [a] que dado um inteiro n e uma lista l calcula a lista sem os (no máximo) n primeiros elementos de l.
dropB :: Int -> [a] -> [a]
dropB x [] = []
dropB x (h:t) = if x==0 then h:t
                else dropB (x-1) t

-- 8 Apresente uma definição recursiva da função (pré-definida) zip :: [a] -> [b] -> [(a,b)] constrói uma lista de pares a partir de duas listas.
zipB :: [a] -> [b] -> [(a,b)]
zipB x [] = []
zipB [] x = []
zipB (x:xs) (y:ys) = (x,y) : zipB xs ys

-- 9 Apresente uma definição recursiva da função (pré-definida) replicate :: Int -> a -> [a] que dado um inteiro n e um elemento x constrói uma lista com n elementos, todos iguais a x.
replicateB :: Int -> a -> [a]
replicateB 0 _ = []
replicateB x y = if x==0 then []
                 else y : replicateB (x-1) y

-- 10 Apresente uma definição recursiva da função (pré-definida) intersperse :: a -> [a] -> [a] que dado um elemento e uma lista, constrói uma lista em que o elemento fornecido é intercalado entre os elementos da lista fornecida.
intersperseB :: a -> [a] -> [a]
intersperseB x [] = []
intersperseB x (y:ys) = if length (y:ys) == 1 then y:ys
                        else y : x : intersperseB x ys

-- 11 Apresente uma definição recursiva da função (pré-definida) group :: Eq a => [a] -> [[a]] que agrupa elementos iguais e consecutivos de uma lista.
groupB :: Eq a => [a] ->[[a]]
groupB [] = []
groupB [h] = [[h]]
groupB (h:t) = let ((x:xs):y) = groupB t
               in if h==x then (h:x:xs):y
                  else [h]:((x:xs):y)

-- 12 Apresente uma definição recursiva da função (pré-definida) concat :: [[a]] -> [a] que concatena as listas de uma lista.
concatB :: [[a]] -> [a]
concatB [] = []
concatB ([]:x) = concatB x
concatB ((x:xs):y) = x : concatB (xs:y)

-- 13 Apresente uma definição recursiva da função (pré-definida) inits :: [a] -> [[a]] que calcula a lista dos prefixos de uma lista.
initsB :: [a] -> [[a]]
initsB [] = [[]]
initsB l = initsB (init l) ++ [l]

-- 14  Apresente uma definição recursiva da função (pré-definida) tails :: [a] -> [[a]] que calcula a lista dos sufixos de uma lista.
tailsB :: [a] -> [[a]]
tailsB [] = [[]]
tailsB l = l : tailsB (tail l)

-- 15  Defina a função heads :: [[a]] -> [a] que recebe uma lista de listas e produz a lista com o primeiro elemento de cada lista.
heads :: [[a]] -> [a]
heads [] = []
heads ([]:x) = heads x
heads (h:t) = head h : heads t

-- 16  Defina a função total :: [[a]] -> Int que recebe uma lista de listas e conta o total de elementos (de todas as listas)
total :: [[a]] -> Int
total [] = 0
total (h:t) = length h + total t

-- 17  Defina a função fun :: [(a,b,c)] -> [(a,c)] que recebe uma lista de triplos e produz a lista de pares com o primeiro e o terceiro elemento de cada triplo.
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((x,y,z):t) = (x,z) : fun t

-- 18  Defina a função cola :: [(String,b,c)] -> String que recebe uma lista de triplos e concatena as strings que estão na primeira componente dos triplos.
cola :: [(String,b,c)] -> String
cola [] = []
cola ((x,_,_):t) = x ++ cola t

-- 19 Defina a função idade :: Int -> Int -> [(String,Int)] -> [String] que recebe o ano, a idade e uma lista de pares com o nome e o ano de nascimento de cada pessoa, e devolve a listas de nomes das pessoas que nesse ano atingirão ou já ultrapassaram a idade indicada.
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade x y ((z,n):t) = if (x-y) >= n then z : idade x y t
                      else idade x y t 

-- 20  Apresente uma definição recursiva da função, powerEnumFrom :: Int -> Int -> [Int] que dado um valor n e um valor m constrói a lista [n^0, . . . , n^m-1].
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom _ 0 = [1]
powerEnumFrom x y = x^y : powerEnumFrom x (y-1)

-- 21  Apresente uma definição recursiva da função, isPrime :: Int -> Bool que dado um número inteiro maior ou igual a 2 determina 
-- se esse número é primo. Para determinar se um número n é primo, descubra se existe algum número inteiro m tal que 2 ≤ m ≤ √n e mod n m = 0. 
-- Se um tal número não existir então n é primo, e se existir então n não é primo.
isPrime :: Int -> Bool
isPrime n | n>=2 = checkDiv n 2
          | otherwise = False

checkDiv :: Int -> Int -> Bool
checkDiv x y | mod x y == 0 = False 
             | y*y > x = True
             | otherwise = checkDiv x (y+1)

-- 22  Apresente uma definição recursiva da função (pré-definida) isPrefixOf :: Eq a => [a] -> [a] -> Bool que testa se uma lista é prefixo de outra.
isPrefixOfB :: Eq a => [a] -> [a] -> Bool
isPrefixOfB [] _ = True
isPrefixOfB (h:t) (x:y) = if h == x then isPrefixOfB t y 
                          else False

-- 23 Apresente uma definição recursiva da função (pré-definida) isSuffixOf :: Eq a => [a] -> [a] -> Bool que testa se uma lista é sufixo de outra.
isSufixOfB :: Eq a => [a] -> [a] -> Bool 
isSufixOfB [] _ = True
isSufixOfB l m = if last l == last m then isSufixOfB (init l) (init m)
                 else False

-- 24  Apresente uma definição recursiva da função (pré-definida) isSubsequenceOf :: Eq a => [a] -> [a] -> Bool que testa se os elementos de uma lista ocorrem noutra pela mesma ordem relativa.                  
isSubsequenceOfB :: Eq a => [a] -> [a] -> Bool
isSubsequenceOfB [] _ = True
isSubsequenceOfB _ [] = False
isSubsequenceOfB (x:xs) (y:ys) = if x == y then isSubsequenceOfB xs ys 
                                 else isSubsequenceOfB (x:xs) ys

-- 25 Apresente uma definição recursiva da função (pré-definida) elemIndices :: Eq a => a -> [a] -> [Int] que calcula a lista de posições em que um dado elemento ocorre numa lista.
elemIndicesB :: Eq a => a -> [a] -> [Int]
elemIndicesB _  [] = []
elemIndicesB x l = idx 0 x l 
                   where idx i x [] = []
                         idx i x (h:t) = if x == h then i : idx (i+1) x t
                                         else idx (i+1) x t

-- 26  Apresente uma definição recursiva da função (pré-definida) nub :: Eq a => [a] -> [a] que calcula uma lista com os mesmos elementos da recebida, sem repetições.
nubB :: Eq a => [a] -> [a]
nubB [] = []
nubB (h:t) = h : filter (/= h) (nubB t)

-- 27  Apresente uma definição recursiva da função (pré-definida) delete :: Eq a => a -> [a] -> [a] que retorna a lista resultante de remover (a primeira ocorrência de) um dado elemento de uma lista.
deleteB :: Eq a => a -> [a] -> [a]
deleteB _ [] = []
deleteB x (h:t) = if h == x then t
                  else h : deleteB x t

-- 28  Apresente uma definição recursiva da função (pré-definida) (\\):: Eq a => [a] -> [a] -> [a] que retorna a lista resultante de remover (as primeiras ocorrências) dos elementos da segunda lista da primeira.
(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) l [] = l
(\\\) [] _ = []
(\\\) l (h:t) = (\\\) (delete h l) t

-- 29 Apresente uma definição recursiva da função (pré-definida) union :: Eq a => [a] -> [a] -> [a] que retorna a lista resultante de acrescentar à primeira lista os elementos da segunda que não ocorrem na primeira.
unionB :: Eq a => [a] -> [a] -> [a]
unionB l [] = l
unionB [] l = l
unionB (x:xs) (y:ys) = if x == y then unionB (x:xs) ys 
                       else x : unionB xs ys ++ [y]

-- 30 Apresente uma definição recursiva da função (pré-definida) intersect :: Eq a => [a] -> [a] -> [a] que retorna a lista resultante de remover da primeira lista os elementos que não pertencem à segunda.
intersectB :: Eq a => [a] -> [a]-> [a]
intersectB [] l = []
intersectB (x:xs) l = if elem x l then x : intersectB xs l 
                      else intersectB xs l

-- 31  Apresente uma definição recursiva da função (pré-definida) insert :: Ord a => a -> [a] -> [a] que dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista.
insertB :: Ord a => a -> [a] -> [a]
insertB x [] = [x]
insertB x (h:t) | x <= h = x : h : t
                | otherwise = h : insertB x t

-- 32  Apresente uma definição recursiva da função (pré-definida) unwords :: [String] -> String que junta todas as strings da lista numa só, separando-as por um espaço.
unwordsB :: [String] -> String
unwordsB [] = ""
unwordsB [x] = x
unwordsB (h:t) = h ++ " " ++ unwordsB t

-- 33  Apresente uma definição recursiva da função (pré-definida) unlines :: [String] -> String que junta todas as strings da lista numa só, separando-as pelo caracter ’\n’.
unlinesB :: [String] -> String
unlinesB [] = ""
unlinesB [x] = x 
unlinesB (h:t) = h ++ "\n" ++ unlinesB t ++ "\n"

--34 Apresente uma definição recursiva da função pMaior :: Ord a => [a] -> Int que dada uma lista não vazia, retorna a posição onde se encontra o maior elemento da lista. As posições da lista começam em 0, i.e., a função deverá retornar 0 se o primeiro elemento da lista for o maior.
pMaiorB :: Ord a => [a] -> Int
pMaiorB (h:t) = idx 0 0 h t 
                where idx i im _ [] = im
                      idx i im x (y:ys) = if x < y then idx (i+1) (i+1) y ys 
                                          else idx (i+1) im x ys

-- 35  Apresente uma definição recursiva da função (pré-definida) lookup :: Eq a => a -> [(a,b)] -> Maybe b que retorna uma lista construída a partir de elementos de uma lista (o segundo argumento) atendendo a uma condição dada pelo primeiro argumento.
lookupB :: Eq a => a -> [(a,b)] -> Maybe b
lookupB x [] = Nothing
lookupB x ((y,z):t) = if x == y then Just z
                     else lookupB x t

-- 36  Defina a função preCrescente :: Ord a => [a] -> [a] calcula o maior prefixo crescente de uma lista.
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:m:t) = if h < m then [h] ++ preCrescente (m:t)
                       else [h]

-- 37 Apresente uma definição recursiva da função iSort :: Ord a => [a] -> [a] que calcula o resultado de ordenar uma lista. Assuma, se precisar, que existe definida a função insert :: Ord a => a -> [a] -> [a] que dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista.
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

-- 38  Apresente uma definição recursiva da função menor :: String -> String -> Bool que dadas duas strings, retorna True se e só se a primeira for menor do que a segunda, segundo a ordem lexicográfica (i.e., do dicionário)
menor :: String -> String -> Bool
menor l [] = False
menor [] l = True
menor (x:xs) (y:ys) | x == y = menor xs ys 
                    | x > y = False
                    | otherwise = True     

-- 39  Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a. Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
-- segunda componente seja menor ou igual a zero. Defina a função elemMSet :: Eq a => a -> [(a,Int)] -> Bool que testa se um elemento pertence a um multi-conjunto. 
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet x ((y,ys):t) | x == y = True
                      | otherwise = elemMSet x t

-- 40 Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a. Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
-- segunda componente seja menor ou igual a zero. Defina a função converteMSet :: [(a,Int)] -> [a] que converte um multi-conjuto na lista dos seus elementos
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,y):t) = replicate y x ++ converteMSet t

-- 41 Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a. Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
-- segunda componente seja menor ou igual a zero. Defina a função insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que acrescenta um elemento a um multi-conjunto.
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]   
insereMSet x [] = [(x,1)]
insereMSet x ((y,z):t) | x == y = (y,z+1):t 
                       | otherwise = insereMSet x t   

-- 42  Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a. Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
-- segunda componente seja menor ou igual a zero. Defina a função removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que remove um
-- elemento a um multi-conjunto. Se o elemento não existir, deve ser retornado o multi-conjunto recebido.                 
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((y,ys):t) = if x == y then t else (y,ys) : removeMSet x t

-- 43 Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a. Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
-- segunda componente seja menor ou igual a zero. Defina a função constroiMSet :: Ord a => [a] -> [(a,Int)] dada uma lista ordenada por ordem crescente, calcula o multi-conjunto dos seus elementos.
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = insereMSet h (constroiMSet t)

-- 44  Apresente uma definição recursiva da função pré-definida partitionEithers :: [Either a b] -> ([a],[b]) que divide uma lista de Either s em duas listas.
partitionEithersB :: [Either a b] -> ([a],[b])
partitionEithersB l = (left l, right l)
                      where left [] = []
                            left (Left x:xs) = x:left xs
                            left (Right x:xs) = left xs 
                            right [] = []
                            right(Left x:xs) = right xs
                            right (Right x:xs)= x: right xs

-- 45 Apresente uma definição recursiva da função pré-definida catMaybes :: [Maybe a] -> [a] que colecciona os elementos do tipo a de uma lista.
catMaybesB :: [Maybe a] -> [a]
catMaybesB [] = []
catMaybesB (h:t) = case h of Nothing -> catMaybesB t
                             Just x -> x: catMaybesB t

-- 46 Considere o seguinte tipo para representar movimentos de um robot.
data Movimento = Norte | Sul | Este | Oeste
                 deriving Show
-- Defina a função caminho :: (Int,Int) -> (Int,Int) -> [Movimento] que, dadas as posições inicial e final (coordenadas) do robot, produz uma lista de movimentos suficientes para que o robot passe de uma posição para a outra.

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) | xi > xf = Oeste : caminho (xi-1,yi) (xf,yf)
                        | xi < xf = Este : caminho (xi+1,yi) (xf,yf)
                        | yi > yf = Sul : caminho (xi,yi-1) (xf,yf)
                        | yi < yf = Norte : caminho (xi,yi+1) (xf,yf)
                        | otherwise = []

-- 47 Considere o seguinte tipo de dados,
-- data Movimento = Norte | Sul | Este | Oeste
--                  deriving Show
-- Defina a função hasLoops :: (Int,Int) -> [Movimento] -> Bool que dada uma posição inicial e uma lista de movimentos (correspondentes a um percurso) verifica se o robot alguma
-- vez volta a passar pela posição inicial ao longo do percurso correspondente. Pode usar a função posicao definida acima.
hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops (x,y) (h:t) = if posicao (x,y) [h] == (x,y) then True
                       else False

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao i [] = i
posicao (x,y) (h:t) = posicao (case h of Norte -> (x, y + 1)
                                         Sul -> (x, y - 1)
                                         Este -> (x + 1, y)
                                         Oeste -> (x - 1, y)) t

-- 48 Considere os seguintes tipos para representar pontos e rectângulos, respectivamente. Assuma que os rectângulos têm os lados paralelos aos eixos e são representados apenas por dois dos pontos mais afastados.
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto
-- Defina a função contaQuadrados :: [Rectangulo] -> Int que, dada uma lista com rectângulos, conta quantos deles são quadrados.
contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (x,y) (z,m)):t) = if abs z-x == abs m-y then 1 + contaQuadrados t 
                                        else contaQuadrados t 

-- 49 Considere os seguintes tipos para representar pontos e rectângulos, respectivamente. Assuma que os rectângulos têm os lados paralelos aos eixos e sâo representados apenas por dois dos pontos mais afastados.
-- Defini a função areaTotal :: [Rectangulo] -> Float que, dada uma lista com rectângulos, determina a área total que eles ocupam.
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x,y) (z,m)):t) = abs (z-x)*(m-y) + areaTotal t 

-- 50  Considere o seguinte tipo para representar o estado de um equipamento.
data Equipamento = Bom | Razoavel | Avariado
                   deriving Show
-- Defina a função naoReparar :: [Equipamento] -> Int que determina a quantidade de equipamentos que não estão avariados.
naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (h:t) = case h of Bom -> 1 + naoReparar t
                             Razoavel -> 1 + naoReparar t
                             Avariado -> naoReparar t


