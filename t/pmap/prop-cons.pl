:- use_module(library(ddata/pmap)).

prop_cons(Map:pmap(integer,integer)) :-
    ( size(Map,0) ->
        \+ cons(_,_,_,Map)  % can't remove from an empty map
    ; otherwise ->
        % extract a single key-value pair
        cons(Key,Value,MapWithout,Map),

        % the mapping existed before
        kv(Map,Key,Val),
        Val == Value,

        % the mapping doesn't exist now
        \+ kv(MapWithout,Key,_)
    ).


:- use_module(library(quickcheck)).
:- use_module(library(tap)).

quickcheck(prop_cons/1).
