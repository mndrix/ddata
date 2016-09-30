:- use_module(library(ddata/pmap)).
:- use_module(library(random), [random_permutation/2]).

prop_insert_exists(Ks:list(integer)) :-
    dedup(Ks,Keys),
    same_length(Keys,Vals),
    maplist(arbitrary(integer),Vals),

    foldl(delta,Keys,Vals,empty,Map),
    maplist(kv(Map),Keys,Got),
    Vals == Got.

dedup(Xs,Unique) :-
    sort(Xs,Sorted),
    random_permutation(Sorted,Unique).

:- use_module(library(quickcheck)).
:- use_module(library(tap)).

quickcheck(prop_insert_exists/1).
