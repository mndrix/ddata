:- use_module(library(ddata/pmap)).

prop_double_insert(Map:pmap(integer,integer),N:integer) :-
    \+ insert_twice(N,Map,_).

insert_twice(N,Map0,Map) :-
    insert(N,foo,Map0,Map1),
    insert(N,foo,Map1,Map).

:- use_module(library(quickcheck)).
:- use_module(library(tap)).

quickcheck(prop_double_insert/2).
