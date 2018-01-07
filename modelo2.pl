:- use_module(library(lists)).
:- use_module(library(clpfd)).

% ex 1
p1(L1,L2) :- % ordena uma lista de forma crescente ou decrescente.
    gen(L1,L2), % copia uma lista.
    test(L2). % verifica se array esta em ordem crescente ou decrescente.

gen([],[]).
gen(L1,[X|L2]) :-
    select(X,L1,L3),
    gen(L3,L2).

test([_,_]).
test([X1,X2,X3|Xs]) :-
    (X1 < X2, X2 < X3; X1 > X2, X2 > X3),
    test([X2,X3|Xs]).

% ex 2
p2(L1,L2) :-
    length(L1,N),
    length(L2,N),
    %
    pos(L1,L2,Is),
    all_distinct(Is),
    %
    labeling([],Is),
    test(L2).

pos([],_,[]).
pos([X|Xs],L2,[I|Is]) :-
    nth1(I,L2,X),
    pos(Xs,L2,Is).

% ex 4
% sweet_recipes(+MaxTime,+NEggs,+RecipeTimes,+RecipeEggs,-Cookings,-Eggs)
sweet_recipes(MaxTime, NEggs, RecipeTimes, RecipeEggs, Cookings, Eggs):-
    Cookings = [A,B,C],
    length(RecipeTimes, N),
    length(RecipeEggs, N),
    domain(Cookings, 1, N),

    % restricoes
    all_distinct(Cookings),

    element(A, RecipeTimes, Times1),
    element(A, RecipeEggs, Eggs1),

    element(B, RecipeTimes, Times2),
    element(B, RecipeEggs, Eggs2),

    element(C, RecipeTimes, Times3),
    element(C, RecipeEggs, Eggs3),


    Times1 + Times2 + Times3 #=< MaxTime,
    Eggs1+Eggs2+Eggs3 #=< NEggs,
    Eggs #= Eggs1+ Eggs2+ Eggs3,


    % eliminar opcoes repetidas.
    A #< B, 
    B #< C,

    % pesquisa
    labeling([maximize(Eggs)], [A,B,C]).
