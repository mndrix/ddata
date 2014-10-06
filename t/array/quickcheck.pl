:- use_module(library(ddata/array)).
:- use_module(library(random), [random_permutation/2]).

prop_insert_exists(Ns:list(positive_integer)) :-
    dedup(Ns,Indices),
    same_length(Indices,Vals),
    maplist(arbitrary(integer),Vals),

    maplist(nth(Arr),Indices,Vals),
    maplist(nth(Arr),Indices,Got),
    Vals == Got.

dedup(Xs,Unique) :-
    sort(Xs,Sorted),
    random_permutation(Sorted,Unique).

:- use_module(library(quickcheck)).
:- use_module(library(tap)).

quickcheck(prop_insert_exists/1).
