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


    
