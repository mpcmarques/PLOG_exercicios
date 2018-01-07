:- use_module(library(clpfd)).

p1(L1, L2):-
    gen(L1, L2),
    test(L2).

gen([], []).
gen(L1, [X|L2]):-
    select(X, L1, L3),
    gen(L3, L2).


p2(L1, L2):-
    length(L1, N),
    length(L2, N),
    domain(L1, 1, 100),
    domain(L2, 1, 100),

    pos(L1,L2, Is),
    all_distinct(Is),

    labeling([], Is).

pos([], _, []).
pos([X|Xs], L2, [I|Is]):-
    nth1(I, L2, X),
    pos(Xs, L2, Is).