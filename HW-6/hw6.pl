redefine_system_predicate(when).
when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).
where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).
enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

schedule(S, P, T) :- enroll(S, X), where(X, P), when(X, T).

usage(P, T) :- where(X, P), when(X, T).

conflict(X, Y) :- X =\= Y, where(X, P), where(Y, P), when(X, T), when(Y, T).

meet(A, B) :- A \== B, enroll(A, X), enroll(B, X).
meet(A, B) :- A \== B, enroll(A, X), enroll(B, Y), where(X, P), where(Y, P), when(X, T1), when(Y, T2), T2 =:= T1 + 1.

rmdup([], []).
rmdup([L | LS], M) :- member(L, LS), rmdup(LS, M).
rmdup([L | LS], [L | MS]) :- rmdup(LS, MS).

flat([], []).
flat(L, [L]).
flat([L | LS], F) :- flat(L, F1), flat(LS, F2), append(F1, F2, F).

get([A | _], 1, A) :- !.
get([_ | AS], N, L) :- N > 1, N1 is N - 1, get(AS, N1, L).

project([], [], []) :- !.
project([], [_ | _], []) :- !.
project([N | NS], P, L) :- get(P, N, L1), project(NS, P, L2), append([L1], L2, L).