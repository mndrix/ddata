:- use_module(library(ddata/pmap)).

% removing all keys gives size 0
prop_remove_all_keys(Map:pmap(atom,atom)) :-
    keys(Map,Keys),
    foldl(delete,Keys,Map,Empty),
    size(Empty,N),
    N == 0.


:- use_module(library(quickcheck)).
:- use_module(library(tap)).

quickcheck(prop_remove_all_keys/1).
