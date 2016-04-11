import Data.Char

--1. Escreva uma função addSuffix :: String -> [String] -> [String] usando list comprehension, para adicionar um dado 
-- sufixo às strings contidas numa lista. Exemplo:

-- > addSuffix "@inf.ufsm.br" ["fulano","beltrano"]
-- ["fulano@inf.ufsm.br","beltrano@inf.ufsm.br]

addSuffix :: String -> [String] -> [String]
addSuffix _ [] = []
addSuffix [] str = str
addSuffix suffix str = [x ++ suffix | x <- str]

--2. Escreva uma função countShorts :: [String] -> Int, que receba uma lista de palavras e retorne a quantidade de palavras
-- dessa lista que possuem menos de 5 caracteres. Use recursão.
countShorts :: [String] -> Int
countShorts [] = 0
countShorts (x:xs) = if (length x < 5) then 1 + countShorts xs else countShorts xs

--3. Reescreva a função do exercício acima, desta vez usando list comprehension.
countShorts' :: [String] -> Int
countShorts' [] = 0
countShorts' lis = length [x | x <- lis, length x < 5]

--4. Escreva uma função ciclo :: Int -> [Int] -> [Int] que receba um número N e uma lista de inteiros, retornando uma nova 
-- lista com N repetições da lista original, conforme o exemplo abaixo:

-- > ciclo 4 [1,3]
-- [1,3,1,3,1,3,1,3]
--Obs.: Você deve usar recursão neste exercício.
ciclo :: Int -> [Int] -> [Int]
ciclo _ [] = []
ciclo 0 _ = []
ciclo n lis = lis ++ ciclo (n - 1) lis

--5. Escreva uma função numera :: [String] -> [(Int,String)], que receba uma lista de palavras e retorne outra lista contendo 
-- tuplas com as palavras numeradas a partir de 1. Use recursão. Exemplo de uso da função:

-- > numera ["abacaxi","mamao","banana"]
-- [(1,"abacaxi"),(2,"mamao"),(3,"banana")]

numera' :: Int -> [String] -> [(Int, String)]
numera' _ [] = []
numera' n (x:xs) = (n, x) : numera' (n + 1) xs

numera :: [String] -> [(Int, String)]
numera [] = []
numera lis = numera' 1 lis

--6. Explique, em forma de comentário, o resultado de cada expressão abaixo.

-- a) [ (x,y) | x <- [1..5], even x, y <- [(x + 1)..6], odd y ]

-- [(2,3),(2,5),(4,5)]
-- gera uma lista de tuplas (x, y) onde x é um número par entre 1 e 5, inclusive, e y é um número ímpar entre x+1 e 6 
-- (3 e 5 no caso e x = 2 e 5 no caso de x = 4)

-- b) [ a ++ b | a <- ["lazy","big"], b <- ["frog", "dog"]]

-- ["lazyfrog","lazydog","bigfrog","bigdog"]
-- gera uma lista com strings resultantes da concatenação de todos os elementos de a com todos os elementos de b

-- c) concat [ [a,'-'] | a <- "paralelepipedo", not (elem a "aeiou")]

-- "p-r-l-l-p-p-d-"
-- gera uma string com todas as vogais de "paralelepípedo" substituídas por "-"

--7. G. Malcolm, Univ. Liverpool) Write a function crossProduct :: [a] -> [b] -> [(a,b)] that takes two lists xs and ys, 
-- and returns the list of all possible pairings:

-- [ (x,y) | x <- xs, y <- ys ]
-- without using the above list comprehension. 
-- (As an exercise in problem decomposition, try first defining a "helper" function pairWithAll :: a -> [b] -> [(a,b)] 
-- that pairs its first argument with each element in its second.)

pairWithAll :: a -> [b] -> [(a, b)]
pairWithAll _ [] = []
pairWithAll x (y:ys) = (x, y) : pairWithAll x ys

crossProduct :: [a] -> [b] -> [(a, b)]
crossProduct _ [] = []
crossProduct [] _ = []
crossProduct x y = pairWithAll (head(x)) y ++ crossProduct (tail(x)) y

--8. Nesta questão você deverá usar list comprehension. Suponha que um retângulo seja representado por uma tupla 
-- (Float,Float,Float,Float), contendo respectivamente as coordenadas x e y do ponto no seu canto superior esquerdo, 
-- seguidas das suas medidas de largura e altura. Sabendo que o eixo x cresce de cima para baixo e o eixo y da 
-- esquerda para direita, crie uma função genRects :: Int -> (Int,Int) -> [(Float,Float,Float,Float)] que receba 
-- um número N e um ponto (x,y) e gere uma sequência de N retângulos não sobrepostos. Os retângulos devem ser alinhados
-- pelos seus topos, a partir do ponto dado, com largura e altura constantes. Por exemplo, usando largura e altura iguais a 5.5:

-- > genRects 3 (0,0) 
-- [(0.0,0.0,5.5,5.5),(5.5,0.0,5.5,5.5),(11.0,0.0,5.5,5.5)]
-- Obs.: Use conversão explícita de tipos quando misturar Int e Float.

genRects :: Int -> (Int, Int) -> [(Float, Float, Float, Float)]
genRects 0 _ = []
genRects n (x, y) = [(fromIntegral x + xs, fromIntegral y, 5.5, 5.5) | xs <- [0.0, 5.5.. 5.5 * (fromIntegral n-1)]]

--9. Escreva uma função recursiva que receba uma lista de tuplas e decomponha cada uma delas, gerando uma tupla de listas, conforme o exemplo abaixo:

-- > func [(1,3),(2,4)]
-- ([1,2], [3,4])

func :: [(Int, Int)] -> ([Int], [Int])
func [] = ([],[])
func (x:xs) = (fst x : fst(func xs), snd x : snd(func xs))

--10. Refaça o exercício anterior usando list comprehension.
func' :: [(Int, Int)] -> ([Int], [Int])
func' [] = ([], [])
func' lis = ([fst x | x <- lis], [snd x | x <- lis])

--11. Refaça o exercício anterior usando uma função de alta ordem.
func'' :: [(Int, Int)] -> ([Int], [Int])
func'' [] = ([], [])
func'' lis = (map fst lis, map snd lis)

--12. O código em validaCPF.hs ilustra a validação dos dígitos verificadores de um CPF. Este código usa let para definir subexpressões, 
-- isto é, expressões intermediárias que irão compor o resultado da função. Observe que este código tem trechos um tanto repetitivos
-- para calcular o primeiro e o segundo dígitos. Você deverá reescrever este código, criando uma função auxiliar que será chamada 
-- 2 vezes dentro de isCpfOk. Nessa função auxiliar, você deverá usar where para definir subexpressões.

{-isCpfOk :: [Int] -> Bool
isCpfOk cpf = 
  let -- calcula primeiro digito
      digitos1 = take 9 cpf
      expr1 = (sum $ zipWith (*) digitos1 [10,9..2]) `mod` 11
      dv1 = if expr1 < 2 then 0 else 11-expr1

      -- calcula segundo digito
      digitos2 = digitos1 ++ [dv1]
      expr2 = (sum $ zipWith (*) digitos2 [11,10..2]) `mod` 11
      dv2 = if expr2 < 2 then 0 else 11-expr2
   in dv1 == cpf !! 9 && dv2 == cpf !! 10

main = do
  let cpf = "12345678909"
      digitos = (map digitToInt cpf)
      result = isCpfOk digitos
  putStrLn (show result)
-}

isCpfOk cpf = 
  let -- calcula primeiro digito
      digitos1 = take 9 cpf
      dv1 = aux 10 digitos1

      -- calcula segundo digito
      digitos2 = digitos1 ++ [dv1]
      dv2 = aux 11 digitos2
   in dv1 == cpf !! 9 && dv2 == cpf !! 10

aux :: Int -> [Int] -> Int
aux n x = if expr < 2 then 0 else 11-expr
	where
		expr = (sum $ zipWith (*) x [n, n-1..2]) `mod` 11

main = do
  let cpf = "12345678909"
      digitos = (map digitToInt cpf)
      result = isCpfOk digitos
  putStrLn (show result)
