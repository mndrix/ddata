:- use_module(library(ddata/pmap)).
:- use_module(library(random), [random_permutation/2]).

% maps unify regardless of the order in which insertions happen
prop_arbitrary_insertion(Map0:pmap(integer,integer)) :-
    pairs(Map0,Pairs0),
    ( Pairs0 = [] ->
        true  % empty maps have no insertions to test
    ; otherwise ->
        random_permutation(Pairs0,Pairs),
        pairs(Map,Pairs),
        Map0 = Map
    ).

:- use_module(library(quickcheck)).
:- use_module(library(tap)).

quickcheck(prop_arbitrary_insertion/1).
