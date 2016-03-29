-- 1.Defina uma função recursiva que receba uma lista de números inteiros e produza uma nova lista com cada número elevado ao quadrado.
doubles :: [Int] -> [Int]
doubles [] = []
doubles (x:xs) = x^2 : doubles xs

--2.Escreva uma função recursiva que receba uma lista de nomes e adicione a string "Sr. " no início de cada nome.
addSr :: [String] -> [String]
addSr [] = []
addSr (x:xs) = ("Sr. " ++ x) : addSr xs

--3.Crie uma função recursiva que receba uma string e retorne o número de espaços nela contidos.
countSpaces :: String -> Int
countSpaces "" = 0
countSpaces (x:xs) = if (x == ' ') then 1 + countSpaces xs else countSpaces xs

--4.Escreva uma função recursiva que, dada uma lista de números, calcule 3*n^2 + 2/n + 1 para cada número n da lista.
calcula :: [Float] -> [Float]
calcula [] = []
calcula (x:xs) = 3*x^2+2/x+1 : calcula xs

--5.Escreva uma função recursiva que, dada uma lista de números, selecione somente os que forem negativos.
negatives :: [Int] -> [Int]
negatives [] = []
negatives (x:xs) = if (x < 0) then x : negatives xs else negatives xs

--6.Defina uma função não-recursiva que receba uma string e retire suas vogais, conforme os exemplos abaixo.

-- > semVogais "andrea"
-- "ndr"
-- > semVogais "xyz"
-- "xyz"
-- > semVogais "ae"
-- ""

semVogais :: String -> String
semVogais x = filter (\x -> x/='a' && x/='e' && x/='i' && x/='i' && x/='o' && x/='u') x

--7.Expresse uma solução recursiva para o exercício anterior.
semVogais' :: String -> String
semVogais' "" = ""
semVogais' (x:xs)
				|x == 'a' = semVogais' xs
				|x == 'e' = semVogais' xs
				|x == 'i' = semVogais' xs
				|x == 'o' = semVogais' xs
				|x == 'u' = semVogais' xs
				|otherwise = x : semVogais' xs

--8.Defina uma função não-recursiva que receba uma string, possivelmente contendo espaços, e que retorne outra string substituindo os demais caracteres por '-', mas mantendo os espaços. Exemplos:

-- > codifica "Rio Grande do Sul"
-- "--- ------ -- ---"
-- > codifica ""
-- ""

codifica :: String -> String
codifica x = map (\n -> if (n == ' ') then ' ' else '-') x

--9.Defina uma função recursiva que resolva o mesmo problema do exercício anterior.
codifica' :: String -> String
codifica' "" = ""
codifica' (x:xs) = if (x == ' ') then x : codifica' xs else '-' : codifica' xs

--10.Crie uma função recursiva charFound :: Char -> String -> Bool, que verifique se o caracter (primeiro argumento) está contido na string (segundo argumento). Exemplos de uso da função:

-- > charFound 'a' ""  
-- False  
-- > charFound 'a' "uau"  
-- True

charFound :: Char -> String -> Bool
charFound _ "" = False
charFound c (x:xs) = if (x == c) then True else charFound c xs

--11. Defina uma função recursiva que receba uma lista de coordenadas de pontos 2D e desloque esses pontos em 2 unidades, conforme o exemplo abaixo:

-- > translate [(0.1,0.2), (1.1,6), (2,3.1)]
-- [(2.1,2.2),(3.1,8.0),(4.0,5.1)]

translate :: [(Float, Float)] -> [(Float, Float)]
translate [] = []
translate (x:xs) = (fst x + 2, snd x + 2) : translate xs

--12.Defina uma função recursiva que receba 2 listas e retorne uma lista contendo o produto, par a par, dos elementos das listas de entrada. Exemplos:

-- > prodVet [1,2,3] [4,5,6]
-- [4,10,18]
-- > prodVet [1,2,3] [4,5,6,7]
-- [4,10,18]

prodVet :: [Int] -> [Int] -> [Int]
prodVet [] _ = []
prodVet _ [] = []
prodVet (x:xs) (y:ys) = x * y : prodVet xs ys

--13. Resolva o exercício anterior usando uma função de alta ordem, eliminando a necessidade de escrever código com recursão.
prodVet' :: [Int] -> [Int] -> [Int]
prodVet' x y = zipWith (*) x y 

--14. Defina uma função recursiva que receba um número n e retorne uma tabela de números de 1 a n e seus quadrados, conforme os exemplos abaixo:

-- > geraTabela 5
-- [(1,1),(2,4),(3,9),(4,16),(5,25)]
-- > geraTabela 0
-- []

geraTabela :: Int -> [(Int, Int)]
geraTabela 0 = []
geraTabela x = geraTabela (x - 1) ++ ((x, x^2) : [])
