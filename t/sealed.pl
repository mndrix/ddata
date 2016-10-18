:- use_module(library(ddata/pmap), []).
:- use_module(library(ddata/map), []).
:- use_module(library(quickcheck)).

prop_sealing_pmap(Map:pmap(integer,integer)) :-
    map:sealed(Map).

:- use_module(library(tap)).

idempotent :-
    map:pairs(Map,[one-1, two-2, three-3]),
    map:sealed(Map),
    map:sealed(Map).


'sealed map has same structure as pmap' :-
    Pairs = [one-1, two-2, three-3],
    pmap:pairs(Pmap,Pairs),
    map:pairs(Map,Pairs),
    map:sealed(Map),

    Map == Pmap.

quickcheck(prop_sealing_pmap/1).
