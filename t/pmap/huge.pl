:- use_module(library(ddata/pmap), []).
:- use_module(library(random), [random/1]).


% generate a big pile of keys for inserting into a map
lots_of_keys(Keys) :-
    length(L,10_000),
    maplist(random,L),
    sort(L,Keys).  % remove duplicates

insert(Key,Map0,Map) :-
    pmap:delta(Key,true,Map0,Map).

:- use_module(library(tap)).

'no collisions' :-
    lots_of_keys(Ks),
    foldl(insert,Ks,empty,_).
