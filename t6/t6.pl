% 1.Defina um predicado zeroInit(L) que é verdadeiro se L for uma lista que inicia com o número 0 (zero). Exemplo de uso:
% ?- zeroInit([9,6,7]).
% false.
% ?- zeroInit([0,6,7]).
% true.

zeroInit([H|_]) :- H =:= 0.

% 2.Defina um predicado has5(L) que é verdadeiro se L for uma lista de 5 elementos. Resolva este exercício sem usar um predicado auxiliar.

has5(L) :- length(L, N), N =:= 5.

% 3.Defina um predicado hasN(L,N) que é verdadeiro se L for uma lista de N elementos.

hasN(L, N) :- length(L, X), N =:= X.

% 4.Defina um predicado potN0(N,L), de forma que L seja uma lista de potências de 2, com expoentes de N a 0. Exemplo de uso:
% ?- potN0(7,L).
% L = [128, 64, 32, 16, 8, 4, 2, 1]

potN0(0, [1]).
potN0(N, L) :-
	N > 0,
	X is 2 ^ N,
	N1 is N - 1,
	L = [X|T], 
	potN0(N1, T).

% 5.Defina um predicado zipmult(L1,L2,L3), de forma que cada elemento da lista L3 seja o produto dos elementos de L1 e 
% L2 na mesma posição do elemento de L3. Exemplo:
% ?­ zipmult([1,2,3],[2,2,2],L). 
% L = [2, 4, 6].

zipmult([], [], []).
zipmult(L1, L2, L3) :- 
	L1 = [H1|T1], 
	L2 = [H2|T2], 
	L3 = [_|T3], 
	N is H1 * H2, 
	L3 = [N|T3], 
	zipmult(T1, T2, T3).

% 6.Defina um predicado potencias(N,L), de forma que L seja uma lista com as N primeiras potências de 2, sendo a primeira 2^0 e assim por diante, 
% conforme o exemplo abaixo:
% ?­ potencias(5,L). 
% L = [1, 2, 4, 8, 16]
% ?­ potencias(0,L).
% L = []
% Dica: defina um predicado auxiliar.

potencias(0, []).
potencias(N, L) :- aux(0, N, L).

aux(N, N, []).
aux(I, N, L) :- 
	I =< N, 
	N1 is 2 ^ I, 
	L = [_|T],
	L = [N1|T],
	N2 is I + 1,
	aux(N2, N, T).
	
% 7.Defina um predicado positivos(L1,L2), de forma que L2 seja uma lista só com os elementos positivos de L1, conforme o exemplo abaixo:
% ?­ positivos([­-1,0,1,-­2,9],L). 
% L = [1, 9]

positivos([], []).
positivos([H|T], L2) :-
	L2 = [H2|T2],
	H > 0,
	H2 = H,
	positivos(T, T2);
	positivos(T, L2).

% 8.Considere que L1 e L2 sejam permutações de uma lista de elementos distintos, sem repetições. Sabendo disso, defina um predicado 
% mesmaPosicao(A,L1,L2) para verificar se um elemento A está na mesma posição nas listas L1 e L2. Exemplo de uso
% ?­ mesmaPosicao(c,[a,b,c,d,e],[e,d,c,b,a]). 
% true
% ?­ mesmaPosicao(b,[a,b,c,d,e],[e,d,c,b,a]).
% false

mesmaPosicao(H, [H|_], [H|_]).
mesmaPosicao(X, [_|T], [_|T1]) :- mesmaPosicao(X, T, T1).

% 9.Dada uma lista de N alunos, deseja-se escolher NP alunos (NP < N) para formar uma comissão. Para isso, defina um predicado 
% comissao(NP,LP,C), que permita gerar as possíveis combinações C com NP elementos da lista LP. Exemplo:
% ?­ comissao(3,[maria,jose,joao,mario],C). 
% C = [maria, jose, joao] ; 
% C = [maria, jose, mario] ; 
% C = [maria, joao, mario] ; 
% C = [jose, joao, mario] 
% ?­ comissao(0,[maria,jose,joao,mario],C).
% C = []

comissaoaux(1, [H|_], [H]).
comissaoaux(N, [_|T], L) :- comissaoaux(N, T, L).
comissaoaux(N, [H|T1], [H|T2]) :- 
	N =\= 1,
	N1 is N - 1,
	comissaoaux(N1, T1, T2).

comissao(NP, LP, C) :- findall(X, comissaoaux(NP, LP, X), C).

% 10.(Adaptado de OBI2006-F1N1) Tem-se N azulejos 10cm x 10cm e, com eles, deve-se montar um conjunto de quadrados de modo a utilizar todos os 
% azulejos dados, sem sobrepô-los. Inicialmente, deve-se montar o maior quadrado possível; então, com os azulejos que sobraram, deve-se montar o maior 
% quadrado possível, e assim sucessivamente. Por exemplo, se forem dados 31 azulejos, o conjunto montado terá 4 quadrados. Para resolver este problema, 
% você deverá definir um predicado azulejos(NA, NQ), de forma que NQ seja o número de quadrados que se deve montar com NA azulejos. Dica: use os predicados sqrt e floor,
% pré-definidos em Prolog.

azulejos(0, 0).
azulejos(NA, NQ) :- 
	X is floor(sqrt(NA)),
	N1 is NA - X * X,
	azulejos(N1, NQ1),
	NQ is NQ1 + 1.