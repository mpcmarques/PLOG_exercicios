:- use_module(library(lists)).
:- use_module(library(clpfd)).

% prof_risco(Pessoa,Profissão)
prof_risco(rui,professor).
prof_risco(to,bombeiro).
prof_risco(marta,bombeiro).
prof_risco(hugo,fuzileiro).

% fuma_muito(Pessoa)
fuma_muito(sofia).
fuma_muito(rui).
fuma_muito(marta).
fuma_muito(pedro).

% dorme(Pessoa,Horas)
dorme(pedro,5).
dorme(joao,4).
dorme(marta,5).
dorme(sofia,8).
dorme(hugo,4).

% 1-a 
dorme_pouco(P):-
    dorme(P, Time),
    Time < 6 .

% 1-b
morre_cedo(P):-
    prof_risco(P, _).
morre_cedo(P):-
    fuma_muito(P), dorme_pouco(P).

% 1-c
% TODO

%1-D
desgracado(P):-
    findall(X, prof_risco(X, _), List),
    member(P, List),
    length(List, N),
    N == 1.

% 1-E
listagem:-
    findall(X, dorme(X, _), List),
    findall(Y, fuma_muito(Y), List2),
    findall(Z, prof_risco(Z, _), List3),

    append(List, List2, ListTemp),
    append(ListTemp, List3, ListFinal),

    remove_dups(ListFinal, ListPrunned),
    write('Nome     Profissao       FumaMuito       DormePouco      MorreCedo'),nl,
    write('=================================================================='), nl,
    listagem_aux(ListPrunned).

listagem_aux([]).
listagem_aux([H |T]):-
    write(H), tab(1),

    (prof_risco(H, Prof) -> (write(Prof), tab(1)) ; (write('-'), tab(1))) ,
    

    (fuma_muito(H) -> (write('X'), tab(1)) ; true),

    (dorme_pouco(H) -> (write('X'),tab(1)) ; true),

    (morre_cedo(H) -> (write('X'), tab(1)) ; true),
    nl,

    listagem_aux(T).

% 2-a
quantidade_de_registros_de_nascimento(N):-
    findall(X, nascimento(X, _, _), List),
    length(List, N).

% 2-b
primeiro_nativo(IdPessoa):-
    nascimento(IdPessoa, _, _), !.

% TODO 2-c e 2d.

% 3 -
% a TODO
a([X],Xs,[[X|Xs]]).
a([X,X|R],Xs,L2) :- !, a([X|R],[X|Xs],L2).
a([X,Y|R],Xs,[[X|Xs]|RL]) :- a([Y|R],[],RL).

% 3-b


% 4- langford
% 4- a
lf(L):-
    length(L, 6),
    domain(L, 1, 3),

    % restricoes
    lf_restricoes(L),
    lf_restricao_numero(1, L),
    lf_restricao_numero(2, L),
    lf_restricao_numero(3, L),

    % pesquisa
    labeling([], L).

lf_restricoes([], _).
lf_restricoes([H|T], L):-
    % cada algarismo ocorre duas vezes na sequencia.
    count(H, L, #=, 2), 
    lf_restricoes(T, L).


lf_restricao_numero(N, L):-
    element(X, L, N),
    Next #= X+N+1,
    Previous #= X-N-1,
    (
        (
            element(Next, L, Direita), 
            element(Previous, L, Esquerda),
            Esquerda #= N, 
            Direita #= N
        ) ;
        (
            element(Next, L, Direita),
            Direita #= N
        ) ;
        (
            element(Previous, L, Esquerda),
            Esquerda #= N
        )
    ).

% 4-b
langford(N, L):-
    FinalLength is 2 * N,
    length(L, FinalLength),
    domain(L, 1, N),

    %
    lf_restricoes(L),
    langford_constrain(N, L),

    %
    labeling([ff], L).

langford_constrain(0,_).
langford_constrain(N, L):-
    lf_restricao_numero(N, L),
    N1 is N-1,
    langford_constrain(N1, L).

    



%  5-
% 1- amarelo, 2- azul, 3- verde, 4- vermelho.
seq(N, Cores, Custo):-
    length(Cores, N),
    domain(Cores, 1, 4),

    % restricoes
    %   nAmarelos > nAzuis
    count(1, Cores, #=, CountAmarelos),
    count(2, Cores, #=, CountAzul),
    CountAmarelos #> CountAzul,

    % nVerdes = 3
    count(3, Cores, #=, CountVerdes),
    CountVerdes #= 3,

    % todos os elementos amarelos tem de estar juntos
    seq_constrain1(Cores),

    % subsequencia azul-verde-amarelo aparece uma vez
    seq_constrain2(Cores),

    count(4, Cores, #=, CountVermelhos),

    Custo #= CountAmarelos * 3 + CountAzul * 1 + CountVerdes * 5 + CountVermelhos * 2,

    % 
    labeling([minimize(Custo)], Cores).

seq_constrain1(Cores):-
    element(X, Cores, 1),
    Next #= X+1,
    Previous #= X-1,
    (element(Next, Cores, 1) ; element(Previous, Cores, 1)).

seq_constrain2(Cores):-
    element(X, Cores, 2),
    Next #= X+1,
    element(Next, Cores, 3),
    Next2 #= Next+1,
    element(Next2, Cores, 1).








