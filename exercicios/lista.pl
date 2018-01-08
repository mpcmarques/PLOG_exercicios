:- use_module(library(clpfd)).
:- use_module(library(lists)).

 
flatten(List, Result):- flatten(List, [], Result).
flatten([], A, A).
flatten([H | T], Temp, Result):-
    (is_list(H) -> flatten(H, Temp, List) ; append(Temp, [H], List)),
    flatten(T, List, Result).

% 1 - Quadrado magico
quadradoMagico3x3(Vars):-
    Vars = [A1,A2,A3,B1,B2,B3,C1,C2,C3],   
    domain(Vars, 1, 9),

    % restricoes
    all_distinct(Vars),
    A1+A2+A3 #= Soma, 
    B1+B2+B3 #= Soma, 
    C1+C2+C3 #= Soma, 
    A1+B1+C1 #= Soma, 
    A2+B2+C2 #= Soma, 
    A3+B3+C3 #= Soma, 
    A1+B2+C3 #= Soma, 
    A3+B2+C1 #= Soma, 

    % pesquisa
    labeling([], Vars).

% 2 - Zebra puzzle
zebraPuzzle(Zebra, Agua):-
    Solucao = [Casas, Nacionalidade, Bebida, Cigarro, Animal],
    Casas = [Vermelha, Amarela, Azul, Verde, Branca],
    Nacionalidade = [Noruegues, Espanhol, Ingles, Ucraniano, Portugues],
    Bebida = [Leite, Cafe, Cha, Sumo, Agua],
    Cigarro = [Marlboro, Chesterfield, Winston, LS, SG],
    Animal = [Cao, Raposa, Cavalo, Iguana, Zebra],
    
    flatten(Solucao, FlattedSol),
    domain(FlattedSol, 1, 5),

    % restricoes
    all_distinct(Casas),
    all_distinct(Nacionalidade),
    all_distinct(Bebida),
    all_distinct(Cigarro),
    all_distinct(Animal),

    Ingles #= Vermelha,
    Espanhol #= Cao,
    Noruegues #= 1,
    Amarela #= Marlboro,
    abs(Chesterfield - Raposa) #= 1,
    abs(Noruegues - Azul) #= 1,
    Winston #= Iguana,
    LS #= Sumo,
    Ucraniano #= Cha,
    Portugues #= SG,
    abs(Marlboro - Cavalo) #= 1,
    Verde #= Cafe, 
    Branca + 1 #= Verde,
    Leite #= 3,


    labeling([], FlattedSol),
    write(Solucao), nl.

% 3 - Problema das rainhas
rainhas4x4(Cols):-
    Cols = [A,B,C,D],
    domain(Cols, 1, 4),

    % restricoes
    all_distinct(Cols),
    A #\= B+1, A #\= B-1, A #\= C+2, A #\= C-2, A #\= D+3, A #\= D-3,
    B #\= C+1, B #\= C-1, B #\= D+2, B #\= D-2,
    C #\= D+1, C #\= D-1,
    
    labeling([], Cols).

% 4 - Criptogramas
puzzle(Vars):-
    Vars = [D, O, N, A, L, G, E, R, B, T],
    domain(Vars, 0, 9),
    
    % restricoes
    all_distinct(Vars),
    D * 100000 + O * 10000 + N * 1000 + A * 100 + L * 10 + D +
    G * 100000 + E * 10000 + R * 1000 + A * 100 + L * 10 + D #=
    R * 100000 + O * 10000 + B * 1000 + E * 100 + R * 10 + T,


    labeling([], Vars).

puzzle2(Vars):-
    Vars = [C,R,O,S,A,D,N,G,E],
    domain(Vars, 0 , 9),

    %restricoes
    all_distinct(Vars),
    C * 10000 + R * 1000 + O * 100 + S * 10 + S +
    R * 1000 + O * 100 + A * 10 + D #=
    D * 100000 + A * 10000 + N * 1000 + G * 100 + E * 10 + R,
  

    labeling([], Vars).

puzzle3(Vars):-
    Vars = [S,E,N,D,M,O,R,Y],
    domain(Vars, 0, 9),

    all_distinct(Vars),
    S * 1000 + E * 100 + N * 10 + D + 
    M * 1000 + O * 100 + R * 10 + E #=
    M * 10000 + O * 1000 + N * 100 + E * 10 + Y,

    labeling([], Vars).

% 6 - Soma e produto
somaProduto(A,B,C):-
    domain([A,B,C], 1, 1000),

    all_distinct([A,B,C]),
    A + B + C #= A * B * C,

    labeling([], [A,B,C]).

% 7 - peru assado
peruAssado(Preco):-
    domain([A,B], 0, 9),

    72 * Preco #=
    A * 1000 + 6 * 100 + 7 * 10 + B,

    labeling([], [A,B,Preco]).
    
% 8 - puto mercearia
putoMercearia(Arroz, Batata, Esparguete, Atum):-
    Vars = [Arroz, Batata, Esparguete, Atum],
    domain(Vars, 1, 100000),

    Arroz + Batata + Esparguete + Atum #= 711,
    Arroz * Batata * Esparguete * Atum #= 711,

    labeling([], Vars).

% 9 - zero zeros
zeroZeros(A,B):-
    domain([A,B], 1, 1000000000),

    A * B #= 1000000000,
    A mod 10 #\= 0,
    B mod 10 #\= 0,
    A #< B,

    labeling([], [A,B]).


    
    