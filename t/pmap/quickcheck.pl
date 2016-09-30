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


% a map's size is the number of keys it has
prop_keys_size(Map:pmap(atom,integer)) :-
    size(Map,MapSize),
    keys(Map,Keys),
    length(Keys,KeysLen),
    KeysLen == MapSize.

:- use_module(library(quickcheck)).
:- use_module(library(tap)).

quickcheck(prop_insert_exists/1).
quickcheck(prop_keys_size/1).
