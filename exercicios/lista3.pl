/* Lista 3 */

/* EX 1
a:  true.
b:  false.
c : H = apple T = [broccoli, refrigerator]
d:  H = a, T = [b,c,d,e]
e:  H = apples T = [bananas]
f: H = a, T = [[b,c,d]]
g: H = apples, T = []
H: false
i: One = apple, Two = sprouts, T = [fridge, milk]
j: X = a, Y = _01, T = _03, Z = [_01 | _03]
K: H = apple, Z = _01, T = [_01]
l: true
*/

/* EX 2
a: false
b: C = [a] , B = [b]
c: X = c
d: false
e: X = joa, Y = gosta, Z = peixe
f: false
g: false
h: P = branco, Q = cavalo.
i: X = 1, Y = 2, Z = 3, D = [4,5,6,7]
*/

% EX 3 - append(L1, L2, L)
append([], L, L).
append([H | L1], L2, [H | L]) :- append(L1, L2, L).

% EX 4 - inverter(L1, L2).
inverter(L1, L2) :-
    inverter(L1, [], L2).

inverter([], V, V).

inverter([H | L1], L2, L3) :-
    inverter(L1, [ H | L2], L3).

% ======= 5
% EX 5a - member(X, L).
member(X, [H | T]) :- 
    (X = H -> true ; member(X, T)).

% EX 5b - member(X, L) utilizando append
member(X, L) :- append(_, [X|_], L).

% EX 5c - last(L, X) - ultimo elemento da lista
last(L, X) :- append(_, [X], L).

% EX 5d - predicado que retorna o n-esimo membro de uma lista
nth_membro(1, [H | _], H).

nth_membro(Index, [_|T], Elemento):-
    Index > 1,
    I1 is Index-1,
    nth_membro(I1, T, Elemento).

% ======== 6
% 6a deleteOne(X, L1, L2)
deleteOne(X, L1, L2) :- append(La,[X | Lb], L1), append(La, Lb, L2).

% 6b delete_all(X, L1, L2)
deleteAll(X, L1, L2) :- 
    deleteOne(X, L1, Lb), 
    deleteAll(X, Lb, L2).

deleteAll(_, A, A).

% 6c delete_all_list(LX, L1, L2)
deleteAllList([H | T], L1, L2) :-
    deleteAll(H, L1, Lb),
    deleteAllList(T, Lb, L2).

deleteAllList(_, A, A).

% ======= 7
% before(X, Y, List)
before(X,Y,L) :- append(_,[X | Lb],L), append(_, [Y |_], Lb).

% ======= 8
% 8a conta(Lista, N)
conta([], 0).
conta([_|T], N) :- 
    N1 is N-1,
    conta(T, N1).

% 8b conta_elem(X, Lista, N)
conta_elem(_, [], 0).

conta_elem(X, [H | T], N):-
    (H = X -> N1 is N-1 ; N1 is N),
    conta_elem(X, T, N1).

%====== 9 TODO 
%9a substitui(X,Y,Lista1,Lista2)
substitui(_,_,[],A,B) :- reverse(A, B).

substitui(X, Y, Lista1, Lista2, ListaF):-
    append(_, [A | Lb], Lista1),
    (A = X -> append([Y], Lista2, Lc) ; append([A], Lista2, Lc)),
    substitui(X, Y, Lb, Lc, ListaF).

substitui(X, Y, Lista1, Lista2):-
   substitui(X, Y, Lista1, [], Lista2).

%9b elimina_duplicados
elimina_duplicados(Lista1, Lista2):-
    elimina_duplicados(Lista1, [], Lista2).

elimina_duplicados([H | T], Lista1, ListaFinal):-
    (member(H, Lista1) -> Lista2 = Lista1 ; append([H], Lista1, Lista2)),
    elimina_duplicados(T, Lista2, ListaFinal).

elimina_duplicados([], A, B) :- reverse(A, B).

% ======== 10
%10 a
ordenada([_]).
ordenada([N1,N2]) :- N1 =< N2.
ordenada([N1, N2 | Resto]):-
    N1 =< N2,
    ordenada([N2 | Resto]).

%10 b
    

