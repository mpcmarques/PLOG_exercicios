:- use_module(library(clpfd)).
:- use_module(library(lists)).

% Problema do Quadrado Mágico

magic(Vars):-
    Vars=[A1,A2,A3,A4,A5,A6,A7,A8,A9],
    domain(Vars,1,9),
    %Soma is (9+1)*3//2,
    all_distinct(Vars),
    A4+A5+A6 #= Soma,
    A7+A8+A9 #= Soma,
    A1+A4+A7 #= Soma,
    A2+A5+A8 #= Soma,
    A3+A6+A9 #= Soma,
    A1+A5+A9 #= Soma,
    A3+A5+A7 #= Soma,
    % A1 #< A2, A1 #< A3, A1 #< A4, A2 #< A4, % Eliminar simetrias 
    labeling([],Vars).

magic4(Vars):-
    Vars=[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16],
    domain(Vars, 1, 16),
    all_distinct(Vars),
    A1+A2+A3+A4 #= Soma,
    A5+A6+A7+A8 #= Soma, 
    A9+A10+A11+A12 #= Soma, 
    A13+A14+A15+A16 #= Soma,
    A1+A6+A11+A16 #= Soma,
    A4+A7+A10+A13 #= Soma,
    labeling([], Vars).

% Zebra puzzle

zebra(Zebra, Agua):-
    % definição das variáveis e seus domínios
    Solucao = [Nacionalidade, Bebida, Tabaco, Animal, Cor],
    Cor = [Vermelha, Azul, Amarela, Branca, Verde],
    Animal = [Cao, Raposa, Iguana, Cavalo, Zebra],
    Nacionalidade = [Ingles, Espanhol, Noruegues, Portugues, Ucraniano],
    Bebida = [Leite, Sumo, Cha, Cafe, Agua],
    Tabaco = [Chesterfield, Winston, LS, SG, Marlboro],
    flatten(Solucao, List),
    domain(List, 1, 5),

    % restricoes
    all_different(Nacionalidade),
    all_different(Animal),
    all_different(Cor),
    all_different(Bebida),
    all_different(Tabaco),
    Ingles #= Vermelha,
    Espanhol #= Cao, 
    Noruegues #= 1,
    Amarela #= Marlboro,
    abs(Chesterfield-Raposa) #= 1,
    abs(Noruegues-Azul) #= 1,
    Winston #= Iguana,
    LS #= Sumo,
    Ucraniano #= Cha, 
    Portugues #= SG, 
    abs(Marlboro-Cavalo) #= 1,
    Verde #= Cafe,
    Verde #= Branca+1,
    Leite #= 3,

    % pesquisa de solução
    labeling([], List),
    write(Solucao), nl.


 
flatten(List, Result):- flatten(List, [], Result).

flatten([], A, A).
flatten([H | T], Temp, Result):-
    (is_list(H) -> flatten(H, Temp, List) ; append(Temp, [H], List)),
    flatten(T, List, Result).


% N- Rainhas 4x4
nrainha(Cols):-
    % definicao das variaveis e seu dominio
    Cols = [A1, A2, A3, A4],
    domain(Cols, 1, 4),

    % restricoes
    all_distinct(Cols),
    A1 #\= A2+1, A1 #\= A2-1, A1 #\= A3+2, A1 #\=A3-2, A1 #\= A4+3, A1 #\= A4-3,
    A2 #\= A3+1, A2 #\= A3-1, A2 #\= A4+2, A2 #\= A4-2,
    A3 #\= A4+1, A3 #\= A4-1,

    % pesquisa
    labeling([], Cols).

nRainha(N, Cols):-
    % definicao das variaveis e seu dominio
    length(Cols, N),
    domain(Cols, 1, 4),
    restricaoNRainha(Cols),
    %all_distinct(Cols),
    labeling([], Cols).

restricaoNRainha([]).
restricaoNRainha([H | Rcols]):-
    restricaoNRainhaAux(H, Rcols, 1).
    restricaoNRainha(Rcols).

restricaoNRainhaAux(_, [], _).
restricaoNRainhaAux(X, [Y | T], K):-
    X #\= Y,
    X + K #\= Y,
    X - K #\= Y,
    K1 is K+1,
    restricaoNRainhaAux(X, T, K1).

% EX. 4 - Criptrogramas
puzzle(Crip):-
    % definir variaveis e seu dominio
    Crip = [D, O, N, A, L, G, E, R, B, T],
    domain(Crip, 1, 10),
    domain([C1,C2,C3,C4, C5, C6], 0, 1),
    all_distinct(Crip),

    % definir restricoes
    D + G #= R + C1 * 10,
    O + E #= O + C2 * 10,
    N + R #= B + C3 * 10, 
    A + A #= E + C4 * 10, 
    L + L #= R + C5 * 10, 
    D + D #= T + C6 * 10,
    

    % fazer pesquisa
    labeling([ff], Crip).


% 6 - Soma e produto
somaProduto(A, B, C):-
    % variaveis e dominios
    domain([A, B, C], 1, 1000),

    % restricoes
    all_different([A,B,C]),
    A + B + C #= A * B * C,

    %pesquisa
    labeling([], [A,B,C]).

% 7- Peru assado
custoPeru(Custo):-
    NumPerus is 72,
    domain([A,B], 1, 9),

    % restricoes
    NumPerus * Custo #= A * 1000 + 7 * 100 + 2  * 10 + B,
    labeling([], [Custo, A, B]).

% 8 - Puto da Mercearia
putoMercearia(Arroz, Batata, Esparguete, Atum):-
    % definir dominio
    domain([Arroz, Batata, Esparguete, Atum], 1, 1000),

    % restricoes
    Arroz + Batata + Esparguete + Atum #= 711,
    Arroz * Batata * Esparguete * Atum #= 711,
    
    (
        (Arroz * Batata) mod 10 #= 0 ;
        (Arroz * Esparguete) mod 10 #= 0 ;  
        (Arroz * Atum) mod 10 #= 0 ;
        (Batata * Esparguete) mod 10 #= 0 ;
        (Batata * Atum) mod 10 #= 0 ;
        (Esparguete * Atum) mod 10 #= 0 
    ),

    Batata #> Atum,
    Atum #> Arroz,
    Arroz #> Esparguete,

    % pesquisa
    labeling([], [Arroz, Batata, Esparguete, Atum]).

% 9 - Zeros Zeros
zerozeros(A,B):-
    domain([A,B], 1, 10000000000),

    % restricoes
    A * B #= 1000000000,
    A mod 10 #\= 0,
    B mod 10 #\= 0, 
    A #\= B,
    A #< B,

    % labeling
    labeling([],[A, B]).
    
% ======= PROBLEMAS DE LOGICA AVANÇADOS ======

% 1 Sequencia magica
sequenciaMagica(N, List):-
    % definir dominio

    length(List, N),
    MaxSize is N-1,
    domain(List, 0, MaxSize),

    % aplicar restricoes
    calculateKV(N, Pairs),
    global_cardinality(List, Pairs),

    % fazer pesquisa
    labeling([], List).

calculateKV(Max, Final):-
   calculateKV([], 0, 0, Max, Final).

calculateKV(Temp, Count, Sum, Max, Final):-
    Sum < Max,
    append(Temp, [Count-Count], Temp2),
    Count1 is Count+1,
    Sum1 is Sum+Count,
    calculateKV(Temp2, Count1, Sum1, Max, Final).
calculateKV(A, _, _, _,A).

% 2 - Hexágono mágico
hexagonoMagico(List):-
    % definir variaveis e dominio
    List = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S],
    domain(List, 1, 19),

    % restricoes
    all_different(List),
    maplist(sum38, [[A,B,C], [D,E,F,G], [H,I,J,K,L], [M,N,O,P], [Q,R,S],
            [H,D,A], [M,I,E,B], [Q,N,J,F,C], [R,O,K,G], [S,P,L],
            [C,G,L], [B,F,K,P], [A,E,J,O,S], [D,I,N,R], [H,M,Q]]),
    


    % calcular
    labeling([], List).

sum38(List):- sum(List, #=, 38).

% 3 - Luzes
/*
luzes(Minumum):-
    % definir variaveis e dominios.
    List = [[A,B,C,D], [E,F,G,H], [I,J,K,L], [M,N,O,P]],
    flatten(List, FlatList),
    domain(FlatList, 0,1),

    transpose(List, CollumnsList),*/

% 5 - Tumografia
tumografia(ListaSomaLinhas, ListaSomaColunas, MatrizFlat):-
    % definir variaveis e dominio
    length(ListaSomaLinhas, NLinhas),
    length(ListaSomaColunas, NColunas),
    criarMatriz(NLinhas, NColunas, MatrizBinaria),
    flatten(MatrizBinaria, MatrizFlat),
    domain(MatrizFlat, 0, 1),

    % definir restricoes
    restricoesTm(MatrizBinaria, ListaSomaLinhas),
    transpose(MatrizBinaria, MatrizBinariaColunas),
    restricoesTm(MatrizBinariaColunas, ListaSomaColunas),

    % fazer pesquisa
    labeling([], MatrizFlat).

    
restricoesTm([], []).
restricoesTm([LinhaBinaria | TBinaria], [HSoma | TSoma]):-
    sum(LinhaBinaria, #=, HSoma),
    restricoesTm(TBinaria, TSoma).

criarMatriz(N, M, [H|T]):-
    N > 0,
    N1 is N-1,
    criarLinha(M, H),
    criarMatriz(N1,M, T).

criarMatriz(0,_, []).

criarLinha(N, [_|T]):-
    N > 0,
    N1 is N-1,
    criarLinha(N1, T).

criarLinha(0, []).

% 6 - Amazons