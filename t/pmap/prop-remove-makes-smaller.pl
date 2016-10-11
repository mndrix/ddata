:- use_module(library(ddata/pmap)).

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

quickcheck(prop_remove_makes_smaller/1).
