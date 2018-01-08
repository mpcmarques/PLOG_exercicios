:- use_module(library(clpfd)).

% 4
% 4-a
square(Vars):-
    Vars = [A1,A2,A3,B1,B2,B3,C1,C2,C3],
    domain(Vars, 1, 9),

    % restricoes
    all_distinct(Vars),
    A1+A2+A3 #= 15,
    B1+B2+B3 #= 15,
    C1+C2+C3 #= 15,

    labeling([], Vars).

% 4-b
square(N, Vars):-
    Length is 9 * N,
    length(Vars, Length),
    % cada celula tem um valor entre 0 e 9.
    domain(Vars, 1, 9),

    % restricoes
    
    % soma dos algarismos de cada uma das linhas devera ser 15.
    sumAlgarisms(Vars),

    % cada algarismo pode aparecer repetido N vezes
    countAlgarism(9, Vars, N),

    % em cada linha os algarismos devem estar ordenados do maior para o menor
    ordemAlgarismos(1, Vars),

    % o primeiro algarismo de duas linhas consecutivas nao pode ser igual
    duasLinhasConsecutivas(1, Vars),

    % pesquisa
    labeling([ff, down], Vars).

duasLinhasConsecutivas(N, Vars):-
    length(Vars, Length),
    N+3 =< Length,

    Next #= N+3,

    element(N, Vars, Element1),
    element(Next, Vars, Element2),
    Element1 #\= Element2,

    NewN is N+3,
    duasLinhasConsecutivas(NewN, Vars).

duasLinhasConsecutivas(_, _).


ordemAlgarismos(N, Vars):-
    length(Vars, Length),
    N+2 =< Length,
    Next #= N+1,
    NextNext #= N+2,

    element(N, Vars, Element1),
    element(Next, Vars, Element2),
    element(NextNext, Vars, Element3),

    % não podem haver algarismos repetidos;
    all_distinct([Element1, Element2, Element3]),
    % os algarismos devem estar ordenados do maior para o mais pequeno
    Element1 #> Element2,
    Element2 #> Element3,

    NewN is N+3,
    ordemAlgarismos(NewN, Vars).

ordemAlgarismos(_, _).


countAlgarism(0, _, _).
countAlgarism(Temp, Vars, N):-
    Temp > 0,
    count(Temp, Vars, #=, N),
    Temp2 is Temp-1,
    countAlgarism(Temp2, Vars, N).

sumAlgarisms(Vars):- sumAlgarisms(1, Vars).
sumAlgarisms(N,Vars):-
        length(Vars, VarsLength),
        N+2 =< VarsLength,

        element(N, Vars, Element1),
        Next #= N+1,
        NextNext #= N+2,
        element(Next, Vars, Element2),
        element(NextNext, Vars, Element3),

        Element1 + Element2 + Element3 #= 15,

        NewN is N+3,
        sumAlgarisms(NewN, Vars).

sumAlgarisms(_, _).


% 5-
seq(Sequence, N, Cost):-
    Length is N*4,
    length(Sequence, Length),
    domain(Sequence, 0, 3),

    % restricoes
    % a primeira peca a fabricar nao pode ser do tipo 0
    element(1, Sequence, PrimeiraPeca), PrimeiraPeca #\= 0,
    % custo
    calculateCost(Sequence, 0, Cost),
    % quantidade a produzir por peca
    count(0, Sequence, #=, N),
    count(1, Sequence, #=, N),
    count(2, Sequence, #=, N),
    count(3, Sequence, #=, N),

    labeling([minimize(Cost)], Sequence).


calculateCost([_], A, A).
calculateCost([Anterior, Current|T] , CurrCost, FinalCost):-
    ((
        Anterior #= 0, 
            (
                ( Current #= 1, NewCost #= CurrCost + 3 );
                ( Current #= 2, NewCost #= CurrCost + 3);
                ( Current #= 3, NewCost #= CurrCost + 2)
            )
    );
    (
        Anterior #= 1,
        Current #= 3, 
        NewCost #= CurrCost + 1
    );
    (
        Anterior #= 2,
        (
            ( Current #= 0, NewCost #= CurrCost+1);
            ( Current #= 1, NewCost #= CurrCost+4)
        )   
    );
    (
        Anterior #= 3,
        (
            ( Current #= 1, NewCost #= CurrCost + 5);
            ( Current #= 3, NewCost #= CurrCost + 2)
        )
    )),
    calculateCost([Current|T], NewCost, FinalCost).