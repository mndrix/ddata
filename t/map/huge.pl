:- use_module(library(ddata/map)).
:- use_module(library(random), [random/1]).


% generate a big pile of keys for inserting into a map
lots_of_keys(Keys) :-
    length(L,10_000),
    maplist(random,L),
    sort(L,Keys).  % remove duplicates

insert(Map,K) :-
    kv(Map,K,true).

:- use_module(library(tap)).

'no duplicates' :-
    lots_of_keys(Ks),
    maplist(insert(_),Ks).
