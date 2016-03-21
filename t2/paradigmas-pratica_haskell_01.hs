--1.Crie uma função somaQuad :: Int -> Int -> Int que calcule a soma dos quadrados de dois números x e y.

somaQuad :: Int -> Int -> Int
somaQuad x y = (x^2) + (y^2)

--2.Crie uma função hasEqHeads :: [Int] -> [Int] -> Bool que verifique se 2 listas possuem o mesmo primeiro elemento. 
--Use o operador lógico '==' para verificar igualdade.

hasEqHeads :: [Int] -> [Int] -> Bool
hasEqHeads lis1 lis2 = if (head lis1) == (head lis2) then True else False

--3.Escreva uma função que receba uma lista de nomes e adicione a string "Sr. " no início de cada nome.
addSr :: [String] -> [String] 
addSr [] = []
addSr lis1 = map("Sr." ++) lis1

--4.Crie uma função que receba uma string e retorne o número de espaços nela contidos. Dica: aplique 2 funções consecutivamente.
countSpaces:: String -> Int
countSpaces x = length (filter (== ' ') x)

--5.Escreva uma função que, dada uma lista de números, calcule 3*n^2 + 2/n + 1 para cada número n da lista. Dica: defina uma função anônima.
calcula :: [Float] -> [Float]
calcula list = map (\x -> 3*x^2 + 2/x + 1) list

--6.Escreva uma função que, dada uma lista de números, selecione somente os que forem negativos.
negatives :: [Int] -> [Int]
negatives list = filter(<0) list

--7.Escreva uma função que receba uma lista de números e retorne somente os que estiverem entre 1 e 100, inclusive. Dica 1: use uma função anônima. 
--Dica 2: use o operador '&&' para expressar um 'E' lógico.
func :: [Int] -> [Int]
func list = filter (\x -> x >= 1 && x <= 100) list

--8.Escreva uma função que, dada uma lista de idades de pessoas no ano atual, retorne uma lista somente com as idades de quem nasceu depois de 1970. 
--Para testar a condição, sua função deverá subtrair a idade do ano atual.
idades :: [Int] -> [Int]
idades list = filter(\x -> 2016 - x > 1970) list

--9.Escreva uma função que receba uma lista de números e retorne somente aqueles que forem pares.
isEven :: [Int] -> [Int]
isEven list = filter(\x -> mod x 2 == 0) list
--10.Crie uma função charFound :: Char -> String -> Bool que verifique se o caracter (primeiro argumento) está contido na string (segundo argumento). 
--Exemplos de uso da função:

-- > charFound 'a' ""  
--False  
-- > charFound 'a' "uau"  
--True  

charFound :: Char -> String -> Bool
charFound x y = (length (filter (== x) y)) > 0

--11.A função takeWhile :: (a -> Bool) -> [a] -> [a] é uma função de alta ordem. Ela recebe uma função condicional e uma lista, retornando o "menor prefixo" 
--(isto é, porção inicial) da lista que satisfaça a condição dada. Teste os exemplos abaixo no GHCi e depois crie um novo exemplo:

-- > takeWhile (< 5) [1,2,3,4,5]
-- > takeWhile (/=' ') "Fulana de Tal"
--Obs.: Este exercício deve ser entregue em forma de comentário.

-- > takeWhile (==5) [5,5,5,5,1,34,5,5,5,5]
--[5,5,5,5]
-- retorna tudo que seja igual a 5, até encontrar um valor diferente

--12.Crie uma função que receba uma lista de nomes e retorne outra lista com somente aqueles nomes que terminarem com a letra 'a'.
finalA :: [String] -> [String]
finalA list = filter(\x -> 'a' == (last x)) list