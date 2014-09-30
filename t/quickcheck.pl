:- use_module(library(hmap)).

prop_insert_exists(Keys:list(integer)) :-
    % TODO remove duplicate keys
    same_length(Keys,Vals),
    maplist(arbitrary(integer),Vals),
    
    maplist(kv(Map),Keys,Vals),
    maplist(kv(Map),Keys,Got),
    Vals == Got.

:- use_module(library(quickcheck)).
:- use_module(library(tap)).

insert_exists(todo) :-
    quickcheck(prop_insert_exists/1).
