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
    (X1 #< X2, X2 #< X3; X1 #> X2, X2 #> X3),
    test([X2,X3|Xs]).

% ex 2
p2(L1,L2) :-
    length(L1, N),
    length(L2, N),
    length(Is, N),
    domain(Is, 0, N),

    % restricoes
    all_distinct(Is),
    pos(L1, L2, Is),
    test(L2),

    labeling([],Is).

pos([],_,[]).
pos([X|Xs],L2,[I|Is]) :-
    element(I,L2,X),
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

% Ex. 5
% corta(+Pranchas,+Prateleiras,-PranchasSelecionadas) 
corta(Pranchas, Prateleiras, PranchasSelecionadas):-
    length(Prateleiras, Nprateleiras),
    length(Pranchas, Npranchas),
    length(PranchasSelecionadas, Nprateleiras),
    domain(PranchasSelecionadas, 1, Npranchas),

    % restricoes - nenhuma prancha e usada de forma excessiva
    getMachines(Pranchas, Machines),
    getTasks(Prateleiras, PranchasSelecionadas, Tasks),
    cumulatives(Tasks, Machines, [bound(upper)]),

    % pesquisa
    labeling([], PranchasSelecionadas).


getMachines(Pranchas, Machines):- getMachines(Pranchas, 1, [], Machines).
getMachines([],_, A, A).
getMachines([Prancha|P], Id, Machines, MachinesFinal):-
    append(Machines, [machine(Id, Prancha)], Machines2),
    Id1 is Id+1,
    getMachines(P, Id1, Machines2, MachinesFinal).

getTasks(Prateleiras, PranchasSelecionadas, Tasks):-
    getTasks(Prateleiras, PranchasSelecionadas, [], Tasks).
getTasks([], [], A, A).
getTasks([CurrPrateleira | Prateleiras], [SelectedPrancha | SelectedPranchas], TasksTemp, Tasks):-
    append(TasksTemp, [task(0, 1, 1, CurrPrateleira, SelectedPrancha)], TasksTemp2),
    getTasks(Prateleiras, SelectedPranchas, TasksTemp2, Tasks).




    


