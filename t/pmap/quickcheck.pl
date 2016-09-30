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


% removing all keys gives size 0
prop_remove_all_keys(Map:pmap(atom,atom)) :-
    keys(Map,Keys),
    foldl(delete,Keys,Map,Empty),
    size(Empty,N),
    N == 0.


% removing a key decreases the size
prop_remove_makes_smaller(Map:pmap(integer,integer)) :-
    size(Map,StartingSize),
    ( StartingSize = 0 ->
        true  % property doesn't apply to empty maps
    ; otherwise ->
        once(kv(Map,Key,_)),
        delete(Key,Map,MapWithout),
        size(MapWithout,EndingSize),
        EndingSize < StartingSize
    ).


:- use_module(library(quickcheck)).
:- use_module(library(tap)).

quickcheck(prop_insert_exists/1).
quickcheck(prop_keys_size/1).
quickcheck(prop_remove_all_keys/1).
quickcheck(prop_remove_makes_smaller/1).
