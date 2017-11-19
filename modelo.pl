:- use_module(library(lists)).

%participant(Id,Age,Performance)
participant(1234, 17, 'Pé coxinho').
participant(3423, 21, 'Programar com os pés').
participant(3788, 20, 'Sing a Bit').
participant(4865, 22, 'Pontes de esparguete').
participant(8937, 19, 'Pontes de pen-drives').
participant(2564, 20, 'Moodle hack').


%performance(Id,Times)
performance(1234,[120,120,120,120]).
performance(3423,[32,120,45,120]).
performance(3788,[110,2,6,43]).
performance(4865,[120,120,110,120]).
performance(8937,[97,101,105,110]).

%madeItThrough(+Participant)
madeItThrough(Participant) :-
    performance(Participant, List),
    member(120, List).


%juriTimes(+Participants, +JuriMember, -Times, -Total)
juriTimes([], _, _, _).

juriTimes([Head | Tail], JuriMember, Times, Total) :-
    number(Total),
    timeForJuri(Head, JuriMember, Time),
    append(Times, [Time], NewTimes),
    NewTotal is Total + Time,
    juriTimes(Tail, JuriMember, NewTimes, NewTotal).

juriTimes(Participants,JuriMember,_,_) :-
    juriTimes(Participants, JuriMember, [], 0).

%timeForJuri(+PerformanceId, +JuriMember, -Time)
timeForJuri(PerformanceId, JuriMember, Time) :-
    performance(PerformanceId, List),
    Index is JuriMember-1,
    nth0(Index, List, Time).

%patientJuri(+JuriMember)

%bestParticipant(+P1, +P2, -P)
bestParticipant(P1, P2, P) :-
    totalTime(P1, TimeP1),
    totalTime(P2, TimeP2),
    TimeP1 \= TimeP2,
    (TimeP1 > TimeP2 -> P is P1; P is P2).

totalTime(Performance, Time) :-
    performance(Performance, List),
    listSum(List, Time).

listSum([], 0).
listSum([H|T], Sum) :-
    listSum(T, Rest),
    Sum is H + Rest.

%allPerfs
allPerfs :-
   perf(X, Y, Z),
   write(X), write(Y), write(Z), nl.


perf(Id, Performance, Times) :-
    performance(Id, Times),
    participant(Id, _, Performance).

%nsuccessfullParticipants(T)
nsuccessfullParticipants(T) :-
    findall(Times, performance(_, Times), L),
    numberOfSuccessfull(L, 0, T).

numberOfSuccessfull([],N, N).
numberOfSuccessfull([H | T], NTimes, NTimesFinal):-
    (isSuccessfull(H) -> NewTimes is NTimes+1 ; NewTimes is NTimes),
    numberOfSuccessfull(T, NewTimes, NTimesFinal).
    
isSuccessfull([]).
isSuccessfull([H | T]) :-
    H = 120,
    isSuccessfull(T).

%juriFans(L)
juriFans(T) :-
    findall(Id, performance(Id, _), IDs),
    findall(Times, performance(_, Times), L),
    juriFansAux(IDs, L, [], L2),
    reverse(L2, T).

juriFansAux(IDs, Times, Lista, FinalList):-
    append(_, [Id | Lc], IDs),
    juriFansAux2(Id, 1, [], Juris),
    append([Id - Juris], Lista, Lista1),
    juriFansAux(Lc, Times, Lista1, FinalList).

juriFansAux(_, _, A, A).

juriFansAux2(Performance, JuriMember, Lista, FinalList):-
    timeForJuri(Performance, JuriMember, Time),
    (Time = 120 -> append([JuriMember],Lista, Lista2) ; Lista2  = Lista),
    JuriMember2 is JuriMember+1,
    juriFansAux2(Performance, JuriMember2, Lista2, FinalList).

juriFansAux2(_, _, B, A) :- reverse(B, A).


%eligibleOutcome(Id,Perf,TT) :-
eligibleOutcome(Id, Perf, TT):-
    performance(Id,Times),
    madeItThrough(Id),
    participant(Id,_,Perf),
    sumlist(Times,TT).

%nextPhase(+N, -Participants)

%impoe(X, L)
impoe(X,L) :-
    length(Mid,X),
    append(L1,[X|_],L), append(_,[X|Mid],L1).


    
