:- use_module(library(ddata/array)).

insert(0,_) :- !.
insert(N,L) :-
    N > 0,
    nth(L,N,true),
    N1 is N - 1,
    insert(N1,L).

:- use_module(library(tap)).

'lots of indices' :-
    insert(100_000,_).
