/* 
Questao 1:
a:
    X = a
    Y = d
    Z = e
b:
    1.
*/

/* 
Questao 2:
 a(X, 2) -> X = A
 b(X, kalamazoo) -> false
 c(X, b3) -> X = a3
 c(X, Y) -> x = a1, Y = b1
*/

/* 
Questao 3:
                                                                  exec(X,Y)
                            p(X, Y)                                                                             s(X)
          q(X), r(Y)                           s(X), r(Y)                                                       X = e
 X = a, Y = c   X = b, Y = c          x = e, Y = c    X = e, Y = d
 X = a, Y = d   X = b, Y = d
*/

%4 - a : fatorial(N, Valor)
fatorial(0, V, V).

fatorial(N, Vinicial, V) :- 
    Nvalor is Vinicial * N,  
    N1 is N-1,
    fatorial(N1, Nvalor, V). 

fatorial(N, Valor) :-
    fatorial(N, 1, Valor).





